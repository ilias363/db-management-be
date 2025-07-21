package ma.ilias.dbmanagementbe.metadata.service.index;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.enums.IndexType;
import ma.ilias.dbmanagementbe.exception.IndexNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.metadata.dto.index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.NewIndexDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.indexcolumn.IndexColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.table.TableService;
import ma.ilias.dbmanagementbe.util.SqlSecurityUtils;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
@Transactional
public class MySqlIndexManager implements IndexService {

    private final JdbcTemplate jdbcTemplate;
    private final TableService tableService;

    @Override
    public Boolean indexExists(String schemaName, String tableName, String indexName) {
        if (!tableService.tableExists(schemaName, tableName)) {
            throw new TableNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase());
        }

        String sql = """
                SELECT COUNT(*) FROM INFORMATION_SCHEMA.STATISTICS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND INDEX_NAME = ?
                """;

        Integer count = jdbcTemplate.queryForObject(sql, Integer.class,
                SqlSecurityUtils.validateSchemaName(schemaName, false),
                SqlSecurityUtils.validateTableName(tableName, false),
                SqlSecurityUtils.validateIndexName(indexName, false));
        return count != null && count > 0;
    }

    @Override
    public IndexMetadataDto getIndex(
            String schemaName, String tableName, String indexName,
            boolean includeTable, boolean checkIndexExists) {
        if (checkIndexExists && !indexExists(schemaName, tableName, indexName)) {
            throw new IndexNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase(), indexName.toLowerCase());
        }

        String indexSql = """
                SELECT INDEX_NAME, NON_UNIQUE, INDEX_TYPE
                FROM INFORMATION_SCHEMA.STATISTICS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND INDEX_NAME = ?
                GROUP BY INDEX_NAME, NON_UNIQUE, INDEX_TYPE
                """;

        String indexColumnsSql = """
                SELECT COLUMN_NAME, SEQ_IN_INDEX, COLLATION
                FROM INFORMATION_SCHEMA.STATISTICS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND INDEX_NAME = ?
                ORDER BY SEQ_IN_INDEX
                """;

        List<IndexColumnMetadataDto> indexColumns = jdbcTemplate.query(
                indexColumnsSql,
                (rs, rowNum) -> {
                    String sortOrder = rs.getString("COLLATION");
                    if (sortOrder != null) {
                        if ("A".equals(sortOrder)) {
                            sortOrder = "ASC";
                        } else if ("D".equals(sortOrder)) {
                            sortOrder = "DESC";
                        }
                    }

                    return IndexColumnMetadataDto.builder()
                            .columnName(rs.getString("COLUMN_NAME"))
                            .ordinalPosition(rs.getInt("SEQ_IN_INDEX"))
                            .sortOrder(sortOrder)
                            .build();
                },
                schemaName, tableName, indexName);

        return jdbcTemplate.queryForObject(
                indexSql,
                (rs, rowNum) -> {
                    String indexTypeStr = rs.getString("INDEX_TYPE");
                    IndexType indexType = null;
                    if (indexTypeStr != null) {
                        try {
                            indexType = IndexType.valueOf(indexTypeStr.toUpperCase());
                        } catch (IllegalArgumentException e) {
                            indexType = null;
                        }
                    }

                    return IndexMetadataDto.builder()
                            .indexName(rs.getString("INDEX_NAME"))
                            .isUnique(!rs.getBoolean("NON_UNIQUE"))
                            .indexType(indexType)
                            .table(includeTable ?
                                    tableService.getTable(schemaName, tableName, true, false, false, false)
                                    : null)
                            .indexColumns(indexColumns)
                            .build();
                },
                schemaName, tableName, indexName);
    }

    @Override
    public List<IndexMetadataDto> getIndexesByTable(
            String schemaName, String tableName,
            boolean includeTable, boolean checkTableExists) {
        if (checkTableExists && !tableService.tableExists(schemaName, tableName)) {
            throw new TableNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase());
        }

        String indexesSql = """
                SELECT INDEX_NAME
                FROM INFORMATION_SCHEMA.STATISTICS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ?
                ORDER BY INDEX_NAME
                """;

        var indexes = jdbcTemplate.query(
                indexesSql,
                (rs, rowNum) -> getIndex(schemaName, tableName, rs.getString("INDEX_NAME"), false, false),
                schemaName, tableName);

        if (includeTable) {
            TableMetadataDto table = tableService.getTable(schemaName, tableName, true, false, false, false);
            indexes.forEach(index -> index.setTable(table));
        }
        return indexes;
    }

    @Override
    public IndexMetadataDto createIndex(NewIndexDto newIndexDto) {
        StringBuilder createIndexSql = new StringBuilder("CREATE ");

        if (Boolean.TRUE.equals(newIndexDto.getIsUnique())) {
            createIndexSql.append("UNIQUE ");
        }

        createIndexSql.append("INDEX ").append(SqlSecurityUtils.validateIndexName(newIndexDto.getIndexName(), true));

        createIndexSql.append(" ON ")
                .append(SqlSecurityUtils.validateSchemaName(newIndexDto.getSchemaName(), true))
                .append(".")
                .append(SqlSecurityUtils.validateTableName(newIndexDto.getTableName(), true))
                .append(" (");

        String columnsPart = newIndexDto.getIndexColumns().stream()
                .map(col -> {
                    StringBuilder colSql = new StringBuilder(SqlSecurityUtils.validateColumnName(col.getColumnName(), true));
                    if (col.getSortOrder() != null && !col.getSortOrder().isBlank()) {
                        colSql.append(" ").append(col.getSortOrder().toUpperCase());
                    }
                    return colSql.toString();
                })
                .collect(Collectors.joining(", "));

        createIndexSql.append(columnsPart).append(")");

        createIndexSql.append(" USING ").append(newIndexDto.getIndexType().toUpperCase());

        jdbcTemplate.execute(createIndexSql.toString());

        return getIndex(newIndexDto.getSchemaName(), newIndexDto.getTableName(), newIndexDto.getIndexName(),
                true, false);
    }

    @Override
    public Boolean deleteIndex(String schemaName, String tableName, String indexName) {
        if (!indexExists(schemaName, tableName, indexName)) {
            throw new IndexNotFoundException(schemaName, tableName, indexName);
        }

        if ("PRIMARY".equalsIgnoreCase(indexName)) {
            throw new UnauthorizedActionException(
                    "Cannot drop PRIMARY KEY index. Use the table's primary key management instead.");

        } else {
            String sql = String.format("DROP INDEX %s ON %s.%s",
                    SqlSecurityUtils.validateIndexName(indexName, true),
                    SqlSecurityUtils.validateSchemaName(schemaName, true),
                    SqlSecurityUtils.validateTableName(tableName, true));
            jdbcTemplate.execute(sql);
        }

        return !indexExists(schemaName, tableName, indexName);
    }
}
