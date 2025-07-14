package ma.ilias.dbmanagementbe.metadata.service.table;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.metadata.dto.Index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.ColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.NewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.foreignkey.ForeignKeyMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.indexcolumn.IndexColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.NewTableDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.UpdateTableDto;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaService;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

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

    @Override
    public List<TableMetadataDto> getTablesBySchema(String schemaName) {
        if (!schemaService.schemaExists(schemaName)) {
            throw new SchemaNotFoundException(schemaName.toLowerCase());
        }

        String tableSql = """
                SELECT TABLE_NAME
                FROM INFORMATION_SCHEMA.TABLES
                WHERE TABLE_SCHEMA = ?
                """;

        return jdbcTemplate.query(
                tableSql,
                ps -> ps.setString(1, schemaName),
                (rs, rowNum) -> getTable(schemaName, rs.getString("TABLE_NAME"))
        );
    }

    @Override
    public TableMetadataDto createTable(NewTableDto newTable) {
        if (!schemaService.schemaExists(newTable.getSchemaName())) {
            throw new SchemaNotFoundException(newTable.getSchemaName().toLowerCase());
        }

        StringBuilder createTableSql = new StringBuilder("CREATE TABLE ")
                .append(newTable.getSchemaName())
                .append(".")
                .append(newTable.getTableName())
                .append(" (");

        Function<NewColumnDto, String> columnSqlBuilder = col -> {
            StringBuilder sb = new StringBuilder();
            sb.append(col.getColumnName()).append(" ").append(col.getDataType());
            if (col.getCharacterMaxLength() != null) {
                sb.append("(").append(col.getCharacterMaxLength()).append(")");
            } else if (col.getNumericPrecision() != null) {
                sb.append("(").append(col.getNumericPrecision());
                if (col.getNumericScale() != null) {
                    sb.append(",").append(col.getNumericScale());
                }
                sb.append(")");
            }
            if (Boolean.TRUE.equals(col.getAutoIncrement())) {
                sb.append(" AUTO_INCREMENT");
            }
            if (Boolean.FALSE.equals(col.getIsNullable())) {
                sb.append(" NOT NULL");
            }
            if (Boolean.TRUE.equals(col.getIsUnique())) {
                sb.append(" UNIQUE");
            }
            if (
                    Boolean.FALSE.equals(col.getAutoIncrement()) &&
                            col.getColumnDefault() != null &&
                            !col.getColumnDefault().isBlank()
            ) {
                if ("CURRENT_TIMESTAMP".equalsIgnoreCase(col.getColumnDefault())) {
                    sb.append(" DEFAULT CURRENT_TIMESTAMP");
                } else {
                    sb.append(" DEFAULT '").append(col.getColumnDefault()).append("'");
                }
            }
            return sb.toString();
        };

        // Add primary key column
        NewColumnDto pkCol = newTable.getPrimaryKey();
        createTableSql.append(columnSqlBuilder.apply(pkCol)).append(" PRIMARY KEY");

        // Add other columns
        if (!newTable.getColumns().isEmpty()) {
            createTableSql.append(", ");
            String columnsSql = newTable.getColumns().stream()
                    .map(columnSqlBuilder)
                    .collect(Collectors.joining(", "));
            createTableSql.append(columnsSql);
        }

        // Add foreign keys if any
        if (newTable.getForeignKeys() != null && !newTable.getForeignKeys().isEmpty()) {
            createTableSql.append(", ");
            String fksSql = newTable.getForeignKeys().stream()
                    .map(fk -> {
                        StringBuilder fkSql = new StringBuilder(String.format(
                                "FOREIGN KEY (%s) REFERENCES %s.%s(%s)",
                                fk.getFromColumnName(),
                                fk.getSchemaName(),
                                fk.getToTableName(),
                                fk.getToColumnName()
                        ));
                        if (fk.getOnUpdateAction() != null && !fk.getOnUpdateAction().isBlank()) {
                            fkSql.append(" ON UPDATE ").append(fk.getOnUpdateAction());
                        }
                        if (fk.getOnDeleteAction() != null && !fk.getOnDeleteAction().isBlank()) {
                            fkSql.append(" ON DELETE ").append(fk.getOnDeleteAction());
                        }
                        return fkSql.toString();
                    })
                    .collect(Collectors.joining(", "));
            createTableSql.append(fksSql);
        }

        createTableSql.append(")");

        jdbcTemplate.execute(createTableSql.toString());

        return getTable(newTable.getSchemaName(), newTable.getTableName());
    }

    @Override
    public TableMetadataDto renameTable(UpdateTableDto updateTableDto) {
        if (!updateTableDto.getTableName().equalsIgnoreCase(updateTableDto.getUpdatedTableName())) {
            String renameSql = String.format("RENAME TABLE %s.%s TO %s.%s",
                    updateTableDto.getSchemaName(), updateTableDto.getTableName(),
                    updateTableDto.getSchemaName(), updateTableDto.getUpdatedTableName());
            jdbcTemplate.execute(renameSql);
        }

        return getTable(updateTableDto.getSchemaName(), updateTableDto.getUpdatedTableName());
    }

    @Override
    public Boolean deleteTable(String schemaName, String tableName, boolean force) {
        if (schemaService.isSystemSchemaByName(schemaName)) {
            throw new UnauthorizedActionException("Cannot delete system table: " + schemaName + "." + tableName);
        }

        if (!tableExists(schemaName, tableName)) {
            throw new TableNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase());
        }

        List<String> fkConstraints = getForeignKeyConstraints(schemaName, tableName);
        if (!force && !fkConstraints.isEmpty()) {
            throw new IllegalStateException("Cannot drop table due to " + fkConstraints.size() + " foreign key constraints. Use force=true to drop constraints automatically.");
        }

        // Drop FK constraints if force=true
        if (force) {
            for (String fk : fkConstraints) {
                String[] parts = fk.split(" ON ");
                String constraintName = parts[0];
                String childTable = parts[1];
                String sql = String.format("ALTER TABLE %s.%s DROP FOREIGN KEY %s", schemaName, childTable, constraintName);
                jdbcTemplate.execute(sql);
            }
        }

        // Drop the table
        String sql = String.format("DROP TABLE %s.%s", schemaName, tableName);
        jdbcTemplate.execute(sql);

        return !tableExists(schemaName, tableName);
    }

    private List<ColumnMetadataDto> queryColumnsForTable(String schemaName, String tableName) {
        String columnsSql = """
                SELECT  c.COLUMN_NAME,
                        c.ORDINAL_POSITION,
                        c.DATA_TYPE,
                        c.CHARACTER_MAXIMUM_LENGTH,
                        c.NUMERIC_PRECISION,
                        c.NUMERIC_SCALE,
                        c.IS_NULLABLE,
                        c.COLUMN_DEFAULT,
                        c.COLUMN_KEY,
                        c.EXTRA
                FROM INFORMATION_SCHEMA.COLUMNS c
                WHERE c.TABLE_SCHEMA = ?
                AND c.TABLE_NAME = ?
                AND c.COLUMN_NAME NOT IN (
                    SELECT kcu.COLUMN_NAME
                    FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE kcu
                    JOIN INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS rc
                    ON kcu.CONSTRAINT_NAME = rc.CONSTRAINT_NAME
                    AND kcu.CONSTRAINT_SCHEMA = rc.CONSTRAINT_SCHEMA
                    WHERE kcu.TABLE_SCHEMA = ?
                    AND kcu.TABLE_NAME = ?
                    AND kcu.REFERENCED_TABLE_NAME IS NOT NULL
                )
                ORDER BY c.ORDINAL_POSITION
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
                    ps.setString(3, schemaName);
                    ps.setString(4, tableName);
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

    private List<String> getForeignKeyConstraints(String schemaName, String tableName) {
        String sql = """
                    SELECT rc.constraint_name, rc.table_name
                    FROM information_schema.referential_constraints rc
                    JOIN information_schema.key_column_usage kcu
                    ON rc.constraint_name = kcu.constraint_name
                    AND rc.constraint_schema = kcu.constraint_schema
                    WHERE rc.referenced_table_name = ?
                    AND rc.constraint_schema = ?
                """;
        return jdbcTemplate.query(sql, (rs, rowNum) ->
                        rs.getString("constraint_name") + " ON " + rs.getString("table_name"),
                tableName, schemaName);
    }
}
