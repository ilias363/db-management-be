package ma.ilias.dbmanagementbe.metadata.service.index;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.enums.IndexType;
import ma.ilias.dbmanagementbe.exception.IndexNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.indexcolumn.IndexColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.table.TableService;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

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

        Integer count = jdbcTemplate.queryForObject(sql, Integer.class, schemaName, tableName, indexName);
        return count != null && count > 0;
    }

    @Override
    public IndexMetadataDto getIndex(
            String schemaName, String tableName, String indexName,
            boolean includeTable, boolean checkIndexExists) {
        if (checkIndexExists && !indexExists(schemaName, tableName, indexName)) {
            throw new IndexNotFoundException(schemaName, tableName, indexName);
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
                            .table(includeTable ? tableService.getTable(schemaName, tableName, false, false) : null)
                            .indexColumns(indexColumns)
                            .build();
                },
                schemaName, tableName, indexName);
    }
}
