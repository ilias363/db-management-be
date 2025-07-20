package ma.ilias.dbmanagementbe.metadata.service.table;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.enums.IndexType;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.ForeignKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.NewPrimaryKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.PrimaryKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.NewStandardColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.StandardColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.indexcolumn.IndexColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.NewTableDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.UpdateTableDto;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaService;

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
                (rs, rowNum) -> rs.getString("TABLE_NAME"));

        return !tables.isEmpty();
    }

    @Override
    public TableMetadataDto getTable(String schemaName, String tableName, boolean includeColumns,
            boolean includeIndexes) {
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
                                        .build())
                        .columns(includeColumns ? queryColumnsForTable(schemaName, tableName) : null)
                        .indexes(includeIndexes ? queryIndexesForTable(schemaName, tableName) : null)
                        .build(),
                schemaName,
                tableName);
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
                (rs, rowNum) -> getTable(schemaName, rs.getString("TABLE_NAME"), true, true));
    }

    @Override
    public TableMetadataDto createTable(NewTableDto newTable) {
        StringBuilder createTableSql = new StringBuilder("CREATE TABLE ")
                .append(newTable.getSchemaName())
                .append(".")
                .append(newTable.getTableName())
                .append(" (");

        Function<BaseNewColumnDto, String> columnSqlBuilder = col -> {
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

            if (col instanceof NewStandardColumnDto standardCol) {
                if (Boolean.FALSE.equals(standardCol.getIsNullable())) {
                    sb.append(" NOT NULL");
                }
                if (Boolean.TRUE.equals(standardCol.getIsUnique())) {
                    sb.append(" UNIQUE");
                }
                if (standardCol.getColumnDefault() != null) {
                    if ("CURRENT_TIMESTAMP".equalsIgnoreCase(standardCol.getColumnDefault())) {
                        sb.append(" DEFAULT CURRENT_TIMESTAMP");
                    } else if ("NULL".equalsIgnoreCase(standardCol.getColumnDefault())) {
                        sb.append(" DEFAULT NULL");
                    } else {
                        sb.append(" DEFAULT '").append(standardCol.getColumnDefault()).append("'");
                    }
                }
            } else if (col instanceof NewPrimaryKeyColumnDto pkCol) {
                if (Boolean.TRUE.equals(pkCol.getAutoIncrement())) {
                    sb.append(" AUTO_INCREMENT");
                }
            }

            return sb.toString();
        };

        if (!newTable.getColumns().isEmpty()) {
            String columnsSql = newTable.getColumns().stream()
                    .map(columnSqlBuilder)
                    .collect(Collectors.joining(", "));
            createTableSql.append(columnsSql);

            // Add primary key constraint
            List<String> primaryKeyColumns = newTable.getColumns().stream()
                    .filter(col -> col.getColumnType() == ColumnType.PRIMARY_KEY
                            || col.getColumnType() == ColumnType.PRIMARY_KEY_FOREIGN_KEY)
                    .map(BaseNewColumnDto::getColumnName)
                    .collect(Collectors.toList());

            if (!primaryKeyColumns.isEmpty()) {
                createTableSql.append(", PRIMARY KEY (")
                        .append(String.join(", ", primaryKeyColumns))
                        .append(") ");
            }

            // Add foreign key constraints
            List<BaseNewColumnDto> foreignKeyColumns = newTable.getColumns().stream()
                    .filter(col -> col.getColumnType() == ColumnType.FOREIGN_KEY
                            || col.getColumnType() == ColumnType.PRIMARY_KEY_FOREIGN_KEY)
                    .collect(Collectors.toList());

            if (!foreignKeyColumns.isEmpty()) {
                createTableSql.append(", ");
                String fksSql = newTable.getColumns().stream()
                        .map(col -> {
                            BaseNewForeignKeyColumnDto fk = (BaseNewForeignKeyColumnDto) col;
                            StringBuilder fkSql = new StringBuilder(String.format(
                                    "FOREIGN KEY (%s) REFERENCES %s.%s(%s)",
                                    fk.getColumnName(),
                                    fk.getReferencedSchemaName(),
                                    fk.getReferencedTableName(),
                                    fk.getReferencedColumnName()));
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
        }

        createTableSql.append(")");

        jdbcTemplate.execute(createTableSql.toString());

        return getTable(newTable.getSchemaName(), newTable.getTableName(), true, true);
    }

    @Override
    public TableMetadataDto renameTable(UpdateTableDto updateTableDto) {
        if (!updateTableDto.getTableName().equalsIgnoreCase(updateTableDto.getUpdatedTableName())) {
            String renameSql = String.format("RENAME TABLE %s.%s TO %s.%s",
                    updateTableDto.getSchemaName(), updateTableDto.getTableName(),
                    updateTableDto.getSchemaName(), updateTableDto.getUpdatedTableName());
            jdbcTemplate.execute(renameSql);
        }

        return getTable(updateTableDto.getSchemaName(), updateTableDto.getUpdatedTableName(), true, true);
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
            throw new UnauthorizedActionException(
                    "Cannot drop table due to " + fkConstraints.size()
                            + " foreign key constraints. Use force=true to drop constraints automatically.");
        }

        // Drop FK constraints if force=true
        if (force) {
            for (String fk : fkConstraints) {
                String[] parts = fk.split(" ON ");
                String constraintName = parts[0];
                String childTable = parts[1];
                String sql = String.format("ALTER TABLE %s.%s DROP FOREIGN KEY %s", schemaName, childTable,
                        constraintName);
                jdbcTemplate.execute(sql);
            }
        }

        // Drop the table
        String sql = String.format("DROP TABLE %s.%s", schemaName, tableName);
        jdbcTemplate.execute(sql);

        return !tableExists(schemaName, tableName);
    }

    private List<BaseColumnMetadataDto> queryColumnsForTable(String schemaName, String tableName) {
        List<BaseColumnMetadataDto> allColumns = new ArrayList<>();

        String allColumnsSql = """
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
                WHERE c.TABLE_SCHEMA = ? AND c.TABLE_NAME = ?
                ORDER BY c.ORDINAL_POSITION
                """;

        // Get foreign key information
        String fkInfoSql = """
                SELECT kcu.COLUMN_NAME,
                       kcu.REFERENCED_TABLE_SCHEMA,
                       kcu.REFERENCED_TABLE_NAME,
                       kcu.REFERENCED_COLUMN_NAME,
                       rc.UPDATE_RULE,
                       rc.DELETE_RULE
                FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE kcu
                JOIN INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS rc
                ON kcu.CONSTRAINT_NAME = rc.CONSTRAINT_NAME
                AND kcu.CONSTRAINT_SCHEMA = rc.CONSTRAINT_SCHEMA
                WHERE kcu.TABLE_SCHEMA = ? AND kcu.TABLE_NAME = ?
                AND kcu.REFERENCED_TABLE_NAME IS NOT NULL
                """;

        // Get unique columns
        String uniqueColsSql = """
                SELECT c.COLUMN_NAME
                FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE c
                JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS t
                  ON c.CONSTRAINT_NAME = t.CONSTRAINT_NAME
                WHERE c.TABLE_SCHEMA = ? AND c.TABLE_NAME = ? AND t.CONSTRAINT_TYPE = 'UNIQUE'
                """;

        // Get foreign key column information
        Map<String, ForeignKeyInfo> foreignKeyMap = new HashMap<>();
        jdbcTemplate.query(fkInfoSql, ps -> {
            ps.setString(1, schemaName);
            ps.setString(2, tableName);
        }, (rs, rowNum) -> {
            String columnName = rs.getString("COLUMN_NAME");
            ForeignKeyInfo fkInfo = new ForeignKeyInfo(
                    rs.getString("REFERENCED_TABLE_SCHEMA"),
                    rs.getString("REFERENCED_TABLE_NAME"),
                    rs.getString("REFERENCED_COLUMN_NAME"),
                    rs.getString("UPDATE_RULE"),
                    rs.getString("DELETE_RULE"));
            foreignKeyMap.put(columnName, fkInfo);
            return null;
        });

        // Get unique columns
        List<String> uniqueColumns = jdbcTemplate.query(uniqueColsSql, ps -> {
            ps.setString(1, schemaName);
            ps.setString(2, tableName);
        }, (rs, rowNum) -> rs.getString("COLUMN_NAME"));

        // Process all columns
        jdbcTemplate.query(allColumnsSql, ps -> {
            ps.setString(1, schemaName);
            ps.setString(2, tableName);
        }, (rs, rowNum) -> {
            String columnName = rs.getString("COLUMN_NAME");
            Integer ordinalPosition = rs.getObject("ORDINAL_POSITION", Integer.class);
            String dataType = rs.getString("DATA_TYPE");
            Long characterMaxLength = rs.getObject("CHARACTER_MAXIMUM_LENGTH", Long.class);
            Integer numericPrecision = rs.getObject("NUMERIC_PRECISION", Integer.class);
            Integer numericScale = rs.getObject("NUMERIC_SCALE", Integer.class);
            String columnDefault = rs.getString("COLUMN_DEFAULT");
            Boolean autoIncrement = rs.getString("EXTRA") != null
                    && rs.getString("EXTRA").toLowerCase().contains("auto_increment");
            boolean isPrimaryKey = "PRI".equalsIgnoreCase(rs.getString("COLUMN_KEY"));
            Boolean isNullable = !isPrimaryKey && "YES".equalsIgnoreCase(rs.getString("IS_NULLABLE"));
            Boolean isUnique = isPrimaryKey || uniqueColumns.contains(columnName);

            BaseColumnMetadataDto column;
            if (isPrimaryKey) {
                column = PrimaryKeyColumnMetadataDto.builder()
                        .columnName(columnName)
                        .ordinalPosition(ordinalPosition)
                        .dataType(dataType)
                        .characterMaxLength(characterMaxLength)
                        .numericPrecision(numericPrecision)
                        .numericScale(numericScale)
                        .isNullable(false)
                        .columnDefault(columnDefault)
                        .autoIncrement(autoIncrement)
                        .isUnique(true)
                        .build();
            } else if (foreignKeyMap.containsKey(columnName)) {
                ForeignKeyInfo fkInfo = foreignKeyMap.get(columnName);
                column = ForeignKeyColumnMetadataDto.builder()
                        .columnName(columnName)
                        .ordinalPosition(ordinalPosition)
                        .dataType(dataType)
                        .characterMaxLength(characterMaxLength)
                        .numericPrecision(numericPrecision)
                        .numericScale(numericScale)
                        .isNullable(isNullable)
                        .columnDefault(columnDefault)
                        .autoIncrement(autoIncrement)
                        .isUnique(isUnique)
                        .referencedSchemaName(fkInfo.referencedSchemaName())
                        .referencedTableName(fkInfo.referencedTableName())
                        .referencedColumnName(fkInfo.referencedColumnName())
                        .onUpdateAction(fkInfo.onUpdateAction())
                        .onDeleteAction(fkInfo.onDeleteAction())
                        .build();
            } else {
                column = StandardColumnMetadataDto.builder()
                        .columnName(columnName)
                        .ordinalPosition(ordinalPosition)
                        .dataType(dataType)
                        .characterMaxLength(characterMaxLength)
                        .numericPrecision(numericPrecision)
                        .numericScale(numericScale)
                        .isNullable(isNullable)
                        .columnDefault(columnDefault)
                        .autoIncrement(autoIncrement)
                        .isUnique(isUnique)
                        .build();
            }

            allColumns.add(column);
            return null;
        });

        return allColumns;
    }

    // Helper record for foreign key information
    private record ForeignKeyInfo(String referencedSchemaName, String referencedTableName,
            String referencedColumnName, String onUpdateAction, String onDeleteAction) {
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
                                    rs.getString("COLLATION") == null ? null
                                            : (rs.getString("COLLATION").equals("A") ? "ASC"
                                                    : (rs.getString("COLLATION").equals("D") ? "DESC"
                                                            : rs.getString("COLLATION"))))
                            .build();
                    indexColumnsMap.computeIfAbsent(idxName, k -> new ArrayList<>()).add(colDto);
                });

        return jdbcTemplate.query(
                indexesSql,
                ps -> {
                    ps.setString(1, schemaName);
                    ps.setString(2, tableName);
                },
                (rs, rowNum) -> {
                    String idxName = rs.getString("INDEX_NAME");
                    String indexTypeStr = rs.getString("INDEX_TYPE");
                    IndexType indexType = indexTypeStr != null ? IndexType.valueOf(indexTypeStr.toUpperCase()) : null;
                    return IndexMetadataDto.builder()
                            .indexName(idxName)
                            .isUnique(!rs.getBoolean("NON_UNIQUE"))
                            .indexType(indexType)
                            .indexColumns(indexColumnsMap.getOrDefault(idxName, List.of()))
                            .build();
                });
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
        return jdbcTemplate.query(sql,
                (rs, rowNum) -> rs.getString("constraint_name") + " ON " + rs.getString("table_name"),
                tableName, schemaName);
    }
}
