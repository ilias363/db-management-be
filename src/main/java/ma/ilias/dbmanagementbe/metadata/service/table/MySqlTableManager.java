package ma.ilias.dbmanagementbe.metadata.service.table;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.NewPrimaryKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.NewStandardColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.NewTableDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.UpdateTableDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.metadata.service.index.IndexService;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaService;
import ma.ilias.dbmanagementbe.util.SqlSecurityUtils;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
@Transactional
public class MySqlTableManager implements TableService {

    private final JdbcTemplate jdbcTemplate;
    private final SchemaService schemaService;
    private final ColumnService columnService;
    private final IndexService indexService;

    @Override
    public Boolean tableExists(String schemaName, String tableName) {
        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(schemaName);
        String validatedTableName = SqlSecurityUtils.validateTableName(tableName);

        if (!schemaService.schemaExists(validatedSchemaName)) {
            throw new SchemaNotFoundException(validatedSchemaName.toLowerCase());
        }

        String tableSql = """
                SELECT TABLE_NAME
                FROM INFORMATION_SCHEMA.TABLES
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ?
                """;

        List<String> tables = jdbcTemplate.query(
                tableSql,
                ps -> {
                    ps.setString(1, validatedSchemaName);
                    ps.setString(2, validatedTableName);
                },
                (rs, rowNum) -> rs.getString("TABLE_NAME"));

        return !tables.isEmpty();
    }

    @Override
    public TableMetadataDto getTable(String schemaName, String tableName, boolean includeSchema,
                                     boolean includeColumns, boolean includeIndexes, boolean checkTableExists) {
        if (checkTableExists && !tableExists(schemaName, tableName)) {
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
                        .schema(includeSchema ? schemaService.getSchemaByName(schemaName, false, false) : null)
                        .columns(includeColumns ? columnService.getColumnsByTable(schemaName, tableName, false, false) : null)
                        .indexes(includeIndexes ? indexService.getIndexesByTable(schemaName, tableName, false, false) : null)
                        .build(),
                schemaName,
                tableName);
    }

    @Override
    public List<TableMetadataDto> getTablesBySchema(String schemaName, boolean includeSchema, boolean includeColumns,
                                                    boolean includeIndexes, boolean checkSchemaExists) {
        if (checkSchemaExists && !schemaService.schemaExists(schemaName)) {
            throw new SchemaNotFoundException(schemaName.toLowerCase());
        }

        String tableSql = """
                SELECT TABLE_NAME
                FROM INFORMATION_SCHEMA.TABLES
                WHERE TABLE_SCHEMA = ?
                """;

        var tables = jdbcTemplate.query(
                tableSql,
                (rs, rowNum) -> getTable(
                        schemaName, rs.getString("TABLE_NAME"),
                        false, includeColumns, includeIndexes, false),
                schemaName);

        if (includeSchema) {
            var schema = schemaService.getSchemaByName(schemaName, false, false);
            tables.forEach(table -> table.setSchema(schema));
        }

        return tables;
    }

    @Override
    public TableMetadataDto createTable(NewTableDto newTable) {
        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(newTable.getSchemaName());
        String validatedTableName = SqlSecurityUtils.validateTableName(newTable.getTableName());

        StringBuilder createTableSql = new StringBuilder("CREATE TABLE ")
                .append(validatedSchemaName)
                .append(".")
                .append(validatedTableName)
                .append(" (");

        Function<BaseNewColumnDto, String> columnSqlBuilder = col -> {
            StringBuilder sb = new StringBuilder();
            String validatedColumnName = SqlSecurityUtils.validateColumnName(col.getColumnName());
            sb.append(validatedColumnName).append(" ").append(col.getDataType());
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
                    .map(col -> SqlSecurityUtils.validateColumnName(col.getColumnName()))
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
                    .toList();

            if (!foreignKeyColumns.isEmpty()) {
                createTableSql.append(", ");
                String fksSql = newTable.getColumns().stream()
                        .map(col -> {
                            BaseNewForeignKeyColumnDto fk = (BaseNewForeignKeyColumnDto) col;

                            StringBuilder fkSql = new StringBuilder(String.format(
                                    "FOREIGN KEY (%s) REFERENCES %s.%s(%s)",
                                    SqlSecurityUtils.validateColumnName(fk.getColumnName()),
                                    SqlSecurityUtils.validateSchemaName(fk.getReferencedSchemaName()),
                                    SqlSecurityUtils.validateTableName(fk.getReferencedTableName()),
                                    SqlSecurityUtils.validateColumnName(fk.getReferencedColumnName())));
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

        return getTable(newTable.getSchemaName(), newTable.getTableName(),
                true, true, true, false);
    }

    @Override
    public TableMetadataDto renameTable(UpdateTableDto updateTableDto) {
        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(updateTableDto.getSchemaName());

        if (!updateTableDto.getTableName().equalsIgnoreCase(updateTableDto.getUpdatedTableName())) {
            String renameSql = String.format("RENAME TABLE %s.%s TO %s.%s",
                    validatedSchemaName, SqlSecurityUtils.validateTableName(updateTableDto.getTableName()),
                    validatedSchemaName, SqlSecurityUtils.validateTableName(updateTableDto.getUpdatedTableName()));
            jdbcTemplate.execute(renameSql);
        }

        return getTable(updateTableDto.getSchemaName(), updateTableDto.getUpdatedTableName(),
                true, true, true, false);
    }

    @Override
    public Boolean deleteTable(String schemaName, String tableName, boolean force) {
        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(schemaName);
        String validatedTableName = SqlSecurityUtils.validateTableName(tableName);

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
                String sql = String.format("ALTER TABLE %s.%s DROP FOREIGN KEY %s",
                        validatedSchemaName,
                        SqlSecurityUtils.validateTableName(childTable),
                        SqlSecurityUtils.validateIdentifier(constraintName, "Constraint name"));

                jdbcTemplate.execute(sql);
            }
        }

        // Drop the table
        String sql = String.format("DROP TABLE %s.%s", validatedSchemaName, validatedTableName);
        jdbcTemplate.execute(sql);

        return !tableExists(schemaName, tableName);
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
