package ma.ilias.dbmanagementbe.metadata.service.table;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.enums.PermissionType;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.NewPrimaryKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.NewStandardColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.NewTableDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.UpdateTableDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.service.DatabaseAuthorizationService;
import ma.ilias.dbmanagementbe.util.AuthorizationUtils;
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
    private final MetadataProviderService metadataProviderService;
    private final DatabaseAuthorizationService databaseAuthorizationService;

    @Override
    public Boolean tableExists(String schemaName, String tableName) {
        return metadataProviderService.tableExists(schemaName, tableName);
    }

    @Override
    public TableMetadataDto getTable(String schemaName, String tableName, boolean includeSchema,
                                     boolean includeColumns, boolean includeIndexes, boolean checkTableExists) {
        return getTable(schemaName, tableName, includeSchema, includeColumns, checkTableExists, checkTableExists, false);
    }

    @Override
    public TableMetadataDto getTable(String schemaName, String tableName, boolean includeSchema, boolean includeColumns,
                                     boolean includeIndexes, boolean checkTableExists, boolean checkAuthorization) {
        if (checkAuthorization) databaseAuthorizationService.checkReadPermission(schemaName, tableName);
        return metadataProviderService.getTable(schemaName, tableName, includeSchema, includeColumns, includeIndexes, checkTableExists);
    }

    @Override
    public List<TableMetadataDto> getTablesBySchema(String schemaName, boolean includeSchema, boolean includeColumns,
                                                    boolean includeIndexes, boolean checkSchemaExists) {
        if (AuthorizationUtils.hasPermission(PermissionType.READ, schemaName, null)) {
            return metadataProviderService.getTablesBySchema(schemaName, includeSchema, includeColumns, includeIndexes, checkSchemaExists);
        }

        List<String> accessibleTables = AuthorizationUtils.getAccessibleTablesInSchema(schemaName);
        return metadataProviderService.getTablesBySchema(schemaName, accessibleTables,
                includeSchema, includeColumns, includeIndexes, checkSchemaExists);

    }

    @Override
    public TableMetadataDto createTable(NewTableDto newTable) {
        databaseAuthorizationService.checkCreatePermission(newTable.getSchemaName(), newTable.getTableName());

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
                        sb.append(" DEFAULT ").append(SqlSecurityUtils.sanitizeDefaultValue(standardCol.getColumnDefault()));
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
                    .toList();

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
                String fksSql = foreignKeyColumns.stream()
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
        databaseAuthorizationService.checkWritePermission(updateTableDto.getSchemaName(), updateTableDto.getTableName());

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
        databaseAuthorizationService.checkDeletePermission(schemaName, tableName);

        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(schemaName);
        String validatedTableName = SqlSecurityUtils.validateTableName(tableName);

        if (metadataProviderService.isSystemSchemaByName(schemaName)) {
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
