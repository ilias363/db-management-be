package ma.ilias.dbmanagementbe.metadata.service.table;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.Index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.ColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.foreignkey.ForeignKeyMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.indexcolumn.IndexColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaService;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

@Service
@AllArgsConstructor
@Transactional
public class MySqlTableManager implements TableService {

    private final JdbcTemplate jdbcTemplate;
    private final SchemaService schemaService;

    @Override
    public Boolean tableExists(String schemaName, String tableName) {
        if (!schemaService.schemaExists(schemaName)) {
            throw new SchemaNotFoundException(schemaName.toLowerCase());
        }

        String tableSql = """
                SELECT TABLE_NAME
                FROM INFORMATION_SCHEMA.TABLES
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ?
                """;

        List<String> tables = jdbcTemplate.query(
                tableSql,
                ps -> {
                    ps.setString(1, schemaName);
                    ps.setString(2, tableName);
                },
                (rs, rowNum) -> rs.getString("TABLE_NAME")
        );

        return !tables.isEmpty();
    }

    @Override
    public TableMetadataDto getTable(String schemaName, String tableName) {
        if (!tableExists(schemaName, tableName)) {
            throw new TableNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase());
        }

        String tableSql = """
                SELECT t.TABLE_NAME,
                       COUNT(c.COLUMN_NAME) AS COLUMN_COUNT,
                       t.TABLE_ROWS,
                       (t.DATA_LENGTH + t.INDEX_LENGTH) AS SIZE_IN_BYTES
                FROM INFORMATION_SCHEMA.TABLES t
                LEFT JOIN INFORMATION_SCHEMA.COLUMNS c
                    ON t.TABLE_SCHEMA = c.TABLE_SCHEMA AND t.TABLE_NAME = c.TABLE_NAME
                WHERE t.TABLE_SCHEMA = ? AND t.TABLE_NAME = ?
                GROUP BY t.TABLE_NAME, t.TABLE_ROWS, t.DATA_LENGTH, t.INDEX_LENGTH
                """;

        return jdbcTemplate.queryForObject(
                tableSql,
                (rs, rowNum) -> TableMetadataDto.builder()
                        .tableName(rs.getString("TABLE_NAME"))
                        .columnCount(rs.getInt("COLUMN_COUNT"))
                        .rowCount(rs.getLong("TABLE_ROWS"))
                        .sizeInBytes(rs.getLong("SIZE_IN_BYTES"))
                        .schema(
                                SchemaMetadataDto.builder()
                                        .schemaName(schemaName.toLowerCase())
                                        .isSystemSchema(schemaService.isSystemSchemaByName(schemaName))
                                        .creationDate(null)
                                        .build()
                        )
                        .columns(queryColumnsForTable(schemaName, tableName))
                        .indexes(queryIndexesForTable(schemaName, tableName))
                        .foreignKeys(queryForeignKeysForTable(schemaName, tableName))
                        .build(),
                schemaName,
                tableName
        );
    }

    private List<ColumnMetadataDto> queryColumnsForTable(String schemaName, String tableName) {
        String columnsSql = """
                SELECT COLUMN_NAME,
                       ORDINAL_POSITION,
                       DATA_TYPE,
                       CHARACTER_MAXIMUM_LENGTH,
                       NUMERIC_PRECISION,
                       NUMERIC_SCALE,
                       IS_NULLABLE,
                       COLUMN_DEFAULT,
                       COLUMN_KEY,
                       EXTRA
                FROM INFORMATION_SCHEMA.COLUMNS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ?
                ORDER BY ORDINAL_POSITION
                """;

        String uniqueColsSql = """
                SELECT c.COLUMN_NAME
                FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE c
                JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS t
                  ON c.CONSTRAINT_NAME = t.CONSTRAINT_NAME
                WHERE c.TABLE_SCHEMA = ? AND c.TABLE_NAME = ? AND t.CONSTRAINT_TYPE = 'UNIQUE'
                """;

        List<String> uniqueColumns = jdbcTemplate.query(
                uniqueColsSql,
                ps -> {
                    ps.setString(1, schemaName);
                    ps.setString(2, tableName);
                },
                (rs, rowNum) -> rs.getString("COLUMN_NAME")
        );

        return jdbcTemplate.query(
                columnsSql,
                ps -> {
                    ps.setString(1, schemaName);
                    ps.setString(2, tableName);
                },
                (rs, rowNum) -> {
                    boolean isPrimaryKey = "PRI".equalsIgnoreCase(rs.getString("COLUMN_KEY"));
                    Boolean isNullable = isPrimaryKey || "YES".equalsIgnoreCase(rs.getString("IS_NULLABLE"));
                    Boolean isUnique = isPrimaryKey || uniqueColumns.contains(rs.getString("COLUMN_NAME"));

                    return ColumnMetadataDto.builder()
                            .columnName(rs.getString("COLUMN_NAME"))
                            .ordinalPosition(rs.getObject("ORDINAL_POSITION", Integer.class))
                            .dataType(rs.getString("DATA_TYPE"))
                            .characterMaxLength(rs.getObject("CHARACTER_MAXIMUM_LENGTH", Integer.class))
                            .numericPrecision(rs.getObject("NUMERIC_PRECISION", Integer.class))
                            .numericScale(rs.getObject("NUMERIC_SCALE", Integer.class))
                            .isNullable(isNullable)
                            .isUnique(isUnique)
                            .columnDefault(rs.getString("COLUMN_DEFAULT"))
                            .isPrimaryKey(isPrimaryKey)
                            .autoIncrement(rs.getString("EXTRA") != null && rs.getString("EXTRA").toLowerCase().contains("auto_increment"))
                            .build();
                }
        );
    }

    private List<IndexMetadataDto> queryIndexesForTable(String schemaName, String tableName) {
        String indexesSql = """
                SELECT INDEX_NAME, NON_UNIQUE, INDEX_TYPE
                FROM INFORMATION_SCHEMA.STATISTICS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ?
                GROUP BY INDEX_NAME, NON_UNIQUE, INDEX_TYPE
                """;

        String indexColumnsSql = """
                SELECT INDEX_NAME, COLUMN_NAME, SEQ_IN_INDEX, COLLATION
                FROM INFORMATION_SCHEMA.STATISTICS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ?
                ORDER BY INDEX_NAME, SEQ_IN_INDEX
                """;

        var indexColumnsMap = new HashMap<String, List<IndexColumnMetadataDto>>();
        jdbcTemplate.query(
                indexColumnsSql,
                ps -> {
                    ps.setString(1, schemaName);
                    ps.setString(2, tableName);
                },
                (rs) -> {
                    String idxName = rs.getString("INDEX_NAME");
                    IndexColumnMetadataDto colDto = IndexColumnMetadataDto.builder()
                            .columnName(rs.getString("COLUMN_NAME"))
                            .ordinalPosition(rs.getInt("SEQ_IN_INDEX"))
                            .sortOrder(
                                    rs.getString("COLLATION") == null ?
                                            null :
                                            (rs.getString("COLLATION").equals("A") ?
                                                    "ASC" : (rs.getString("COLLATION").equals("D") ?
                                                    "DESC" : rs.getString("COLLATION"))
                                            )
                            )
                            .build();
                    indexColumnsMap.computeIfAbsent(idxName, k -> new ArrayList<>()).add(colDto);
                }
        );

        return jdbcTemplate.query(
                indexesSql,
                ps -> {
                    ps.setString(1, schemaName);
                    ps.setString(2, tableName);
                },
                (rs, rowNum) -> {
                    String idxName = rs.getString("INDEX_NAME");
                    return IndexMetadataDto.builder()
                            .indexName(idxName)
                            .isUnique(!rs.getBoolean("NON_UNIQUE"))
                            .indexType(rs.getString("INDEX_TYPE"))
                            .indexColumns(indexColumnsMap.getOrDefault(idxName, List.of()))
                            .build();
                }
        );
    }

    private List<ForeignKeyMetadataDto> queryForeignKeysForTable(String schemaName, String tableName) {
        String fkSql = """
                SELECT kcu.CONSTRAINT_NAME,
                       kcu.COLUMN_NAME as SOURCE_COLUMN,
                       kcu.REFERENCED_COLUMN_NAME AS TARGET_COLUMN,
                       kcu.REFERENCED_TABLE_NAME AS TARGET_TABLE,
                       rc.UPDATE_RULE,
                       rc.DELETE_RULE
                FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE kcu
                JOIN INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS rc
                    ON kcu.CONSTRAINT_NAME = rc.CONSTRAINT_NAME
                           AND kcu.TABLE_SCHEMA = rc.CONSTRAINT_SCHEMA
                WHERE kcu.TABLE_SCHEMA = ?
                  AND kcu.TABLE_NAME = ?
                  AND kcu.REFERENCED_TABLE_NAME IS NOT NULL
                """;

        return jdbcTemplate.query(
                fkSql,
                ps -> {
                    ps.setString(1, schemaName);
                    ps.setString(2, tableName);
                },
                (rs, rowNum) -> ForeignKeyMetadataDto.builder()
                        .constraintName(rs.getString("CONSTRAINT_NAME"))
                        .fromColumnName(rs.getString("SOURCE_COLUMN"))
                        .toTableName(rs.getString("TARGET_TABLE"))
                        .toColumnName(rs.getString("TARGET_COLUMN"))
                        .onUpdateAction(rs.getString("UPDATE_RULE"))
                        .onDeleteAction(rs.getString("DELETE_RULE"))
                        .build()
        );
    }
}
