package ma.ilias.dbmanagementbe.metadata.service.column;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.ColumnNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.ForeignKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.PrimaryKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.StandardColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaService;
import ma.ilias.dbmanagementbe.metadata.service.table.TableService;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@AllArgsConstructor
@Transactional
public class MySqlColumnManager implements ColumnService {

    private final JdbcTemplate jdbcTemplate;
    private final SchemaService schemaService;
    private final TableService tableService;

    @Override
    public Boolean columnExists(String schemaName, String tableName, String columnName) {
        if (!tableService.tableExists(schemaName, tableName)) {
            throw new TableNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase());
        }

        String sql = """
                SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND COLUMN_NAME = ?
                """;
        Integer count = jdbcTemplate.queryForObject(sql, Integer.class, schemaName, tableName, columnName);
        return count != null && count > 0;
    }

    @Override
    public BaseColumnMetadataDto getColumn(String schemaName, String tableName, String columnName) {
        if (!columnExists(schemaName, tableName, columnName)) {
            throw new ColumnNotFoundException(schemaName, tableName, columnName);
        }

        String sql = """
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
                WHERE c.TABLE_SCHEMA = ? AND c.TABLE_NAME = ? AND c.COLUMN_NAME = ?
                """;

        String uniqueColsSql = """
                SELECT c.COLUMN_NAME
                FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE c
                JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS t
                  ON c.CONSTRAINT_NAME = t.CONSTRAINT_NAME
                WHERE c.TABLE_SCHEMA = ? AND c.TABLE_NAME = ? AND t.CONSTRAINT_TYPE = 'UNIQUE'
                  AND c.COLUMN_NAME = ?
                """;

        List<String> uniqueColumns = jdbcTemplate.query(
                uniqueColsSql,
                ps -> {
                    ps.setString(1, schemaName);
                    ps.setString(2, tableName);
                    ps.setString(3, columnName);
                },
                (rs, rowNum) -> rs.getString("COLUMN_NAME")
        );

        // Get foreign key information
        String foreignKeySql = """
                SELECT kcu.REFERENCED_TABLE_SCHEMA,
                       kcu.REFERENCED_TABLE_NAME,
                       kcu.REFERENCED_COLUMN_NAME,
                       rc.UPDATE_RULE,
                       rc.DELETE_RULE
                FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE kcu
                JOIN INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS rc
                    ON kcu.CONSTRAINT_NAME = rc.CONSTRAINT_NAME
                    AND kcu.CONSTRAINT_SCHEMA = rc.CONSTRAINT_SCHEMA
                WHERE kcu.TABLE_SCHEMA = ? AND kcu.TABLE_NAME = ? AND kcu.COLUMN_NAME = ?
                """;

        List<ForeignKeyInfo> foreignKeyInfos = jdbcTemplate.query(
                foreignKeySql,
                ps -> {
                    ps.setString(1, schemaName);
                    ps.setString(2, tableName);
                    ps.setString(3, columnName);
                },
                (rs, rowNum) -> new ForeignKeyInfo(
                        rs.getString("REFERENCED_TABLE_SCHEMA"),
                        rs.getString("REFERENCED_TABLE_NAME"),
                        rs.getString("REFERENCED_COLUMN_NAME"),
                        rs.getString("DELETE_RULE"),
                        rs.getString("UPDATE_RULE")
                )
        );

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

        TableMetadataDto table = jdbcTemplate.queryForObject(
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
                        .build(),
                schemaName,
                tableName
        );

        return jdbcTemplate.queryForObject(
                sql,
                (rs, rowNum) -> {
                    String colName = rs.getString("COLUMN_NAME");
                    Integer ordinalPosition = rs.getObject("ORDINAL_POSITION", Integer.class);
                    String dataType = rs.getString("DATA_TYPE");
                    Long characterMaxLength = rs.getObject("CHARACTER_MAXIMUM_LENGTH", Long.class);
                    Integer numericPrecision = rs.getObject("NUMERIC_PRECISION", Integer.class);
                    Integer numericScale = rs.getObject("NUMERIC_SCALE", Integer.class);
                    String columnDefault = rs.getString("COLUMN_DEFAULT");
                    Boolean autoIncrement = rs.getString("EXTRA") != null && rs.getString("EXTRA").toLowerCase().contains("auto_increment");
                    boolean isPrimaryKey = "PRI".equalsIgnoreCase(rs.getString("COLUMN_KEY"));
                    Boolean isNullable = !isPrimaryKey && "YES".equalsIgnoreCase(rs.getString("IS_NULLABLE"));
                    Boolean isUnique = isPrimaryKey || uniqueColumns.contains(colName);

                    if (isPrimaryKey) {
                        return PrimaryKeyColumnMetadataDto.builder()
                                .columnName(colName)
                                .ordinalPosition(ordinalPosition)
                                .dataType(dataType)
                                .characterMaxLength(characterMaxLength)
                                .numericPrecision(numericPrecision)
                                .numericScale(numericScale)
                                .isNullable(false)
                                .isUnique(true)
                                .columnDefault(columnDefault)
                                .autoIncrement(autoIncrement)
                                .table(table)
                                .build();
                    } else if (!foreignKeyInfos.isEmpty()) {
                        ForeignKeyInfo fkInfo = foreignKeyInfos.get(0);
                        return ForeignKeyColumnMetadataDto.builder()
                                .columnName(colName)
                                .ordinalPosition(ordinalPosition)
                                .dataType(dataType)
                                .characterMaxLength(characterMaxLength)
                                .numericPrecision(numericPrecision)
                                .numericScale(numericScale)
                                .isNullable(isNullable)
                                .isUnique(isUnique)
                                .columnDefault(columnDefault)
                                .autoIncrement(autoIncrement)
                                .table(table)
                                .referencedSchemaName(fkInfo.referencedSchemaName())
                                .referencedTableName(fkInfo.referencedTableName())
                                .referencedColumnName(fkInfo.referencedColumnName())
                                .onDeleteAction(fkInfo.onDeleteAction())
                                .onUpdateAction(fkInfo.onUpdateAction())
                                .build();
                    } else {
                        return StandardColumnMetadataDto.builder()
                                .columnName(colName)
                                .ordinalPosition(ordinalPosition)
                                .dataType(dataType)
                                .characterMaxLength(characterMaxLength)
                                .numericPrecision(numericPrecision)
                                .numericScale(numericScale)
                                .isNullable(isNullable)
                                .isUnique(isUnique)
                                .columnDefault(columnDefault)
                                .autoIncrement(autoIncrement)
                                .table(table)
                                .build();
                    }
                },
                schemaName,
                tableName,
                columnName
        );
    }

    @Override
    public BaseColumnMetadataDto createColumn(BaseNewColumnDto newColumnDto) {
        StringBuilder alterSql = new StringBuilder("ALTER TABLE ")
                .append(newColumnDto.getSchemaName())
                .append(".")
                .append(newColumnDto.getTableName())
                .append(" ADD COLUMN ")
                .append(newColumnDto.getColumnName())
                .append(" ")
                .append(newColumnDto.getDataType());

        if (newColumnDto.getCharacterMaxLength() != null) {
            alterSql.append("(").append(newColumnDto.getCharacterMaxLength()).append(")");
        } else if (newColumnDto.getNumericPrecision() != null) {
            alterSql.append("(").append(newColumnDto.getNumericPrecision());
            if (newColumnDto.getNumericScale() != null) {
                alterSql.append(",").append(newColumnDto.getNumericScale());
            }
            alterSql.append(")");
        }

        // Add column constraints
        if (Boolean.TRUE.equals(newColumnDto.getAutoIncrement())) {
            alterSql.append(" AUTO_INCREMENT");
        }

        if (Boolean.FALSE.equals(newColumnDto.getIsNullable())) {
            alterSql.append(" NOT NULL");
        }

        if (Boolean.TRUE.equals(newColumnDto.getIsUnique())) {
            alterSql.append(" UNIQUE");
        }

        if (Boolean.FALSE.equals(newColumnDto.getAutoIncrement()) &&
                newColumnDto.getColumnDefault() != null &&
                !newColumnDto.getColumnDefault().isBlank()) {
            if ("CURRENT_TIMESTAMP".equalsIgnoreCase(newColumnDto.getColumnDefault())) {
                alterSql.append(" DEFAULT CURRENT_TIMESTAMP");
            } else {
                alterSql.append(" DEFAULT '").append(newColumnDto.getColumnDefault()).append("'");
            }
        }

        jdbcTemplate.execute(alterSql.toString());

        // Handle foreign key constraint if it's a foreign key column
        if (newColumnDto instanceof NewForeignKeyColumnDto fkDto) {
            if (fkDto.getColumnDefault() != null && !fkDto.getColumnDefault().isBlank()) {
                String updateSql = String.format("UPDATE %s.%s SET %s = ?",
                        fkDto.getSchemaName(), fkDto.getTableName(), fkDto.getColumnName());
                jdbcTemplate.update(updateSql, fkDto.getColumnDefault());
            }

            String fkConstraintSql = String.format(
                    "ALTER TABLE %s.%s ADD CONSTRAINT fk_%s_%s FOREIGN KEY (%s) REFERENCES %s.%s(%s)",
                    newColumnDto.getSchemaName(), newColumnDto.getTableName(), newColumnDto.getTableName(), newColumnDto.getColumnName(),
                    newColumnDto.getColumnName(), fkDto.getReferencedSchemaName(),
                    fkDto.getReferencedTableName(), fkDto.getReferencedColumnName()
            );

            if (fkDto.getOnUpdateAction() != null && !fkDto.getOnUpdateAction().isBlank()) {
                fkConstraintSql += " ON UPDATE " + fkDto.getOnUpdateAction();
            }
            if (fkDto.getOnDeleteAction() != null && !fkDto.getOnDeleteAction().isBlank()) {
                fkConstraintSql += " ON DELETE " + fkDto.getOnDeleteAction();
            }

            jdbcTemplate.execute(fkConstraintSql);
        }

        return getColumn(newColumnDto.getSchemaName(), newColumnDto.getTableName(), newColumnDto.getColumnName());
    }

    @Override
    public Boolean deleteColumn(String schemaName, String tableName, String columnName, boolean force) {
        if (!columnExists(schemaName, tableName, columnName)) {
            throw new ColumnNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase(), columnName.toLowerCase());
        }

        if (schemaService.isSystemSchemaByName(schemaName)) {
            throw new UnauthorizedActionException("Cannot delete column from system table: " + schemaName + "." + tableName);
        }

        if (tableService.getTable(schemaName, tableName).getColumnCount() == 1) {
            throw new UnauthorizedActionException("Table " + tableName + " has only one column. Try dropping the whole table.");
        }

        boolean isPrimaryKey = isColumnPrimaryKey(schemaName, tableName, columnName);

        if (isPrimaryKey && !force) {
            throw new UnauthorizedActionException("Cannot delete primary key column. Use force=true to proceed.");
        }

        // Check if the column is part of a foreign key constraint
        String fkCheckSql = """
                SELECT CONSTRAINT_NAME
                FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND COLUMN_NAME = ?
                  AND REFERENCED_TABLE_NAME IS NOT NULL
                """;

        List<String> fkConstraints = jdbcTemplate.query(
                fkCheckSql,
                (rs, rowNum) -> rs.getString("CONSTRAINT_NAME"),
                schemaName,
                tableName,
                columnName
        );

        if (!force && !fkConstraints.isEmpty()) {
            throw new UnauthorizedActionException(
                    "Cannot delete column used in a foreign key constraint. Use force=true to drop the constraint automatically."
            );
        }

        if (force && !fkConstraints.isEmpty()) {
            for (String fkConstraint : fkConstraints) {
                String sql = String.format("ALTER TABLE %s.%s DROP FOREIGN KEY %s", schemaName, tableName, fkConstraint);
                jdbcTemplate.execute(sql);
            }
        }

        if (isPrimaryKey) {
            List<String> referencingFkConstraints = getReferencingForeignKeyConstraints(schemaName, tableName, columnName);
            for (String fkConstraint : referencingFkConstraints) {
                String[] parts = fkConstraint.split(" ON ");
                String constraintName = parts[0];
                String referencingTableName = parts[1];
                String dropFkSql = String.format("ALTER TABLE %s.%s DROP FOREIGN KEY %s", schemaName, referencingTableName, constraintName);
                jdbcTemplate.execute(dropFkSql);
            }
        }

        String alterSql = String.format("ALTER TABLE %s.%s DROP COLUMN %s", schemaName, tableName, columnName);
        jdbcTemplate.execute(alterSql);

        return !columnExists(schemaName, tableName, columnName);
    }

    @Override
    public Boolean isColumnPrimaryKey(String schemaName, String tableName, String columnName) {
        String pkCheckSql = """
                SELECT COUNT(*) FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND COLUMN_NAME = ? 
                  AND CONSTRAINT_NAME = 'PRIMARY'
                """;

        Integer pkCount = jdbcTemplate.queryForObject(pkCheckSql, Integer.class, schemaName, tableName, columnName);
        return pkCount != null && pkCount > 0;
    }

    private List<String> getReferencingForeignKeyConstraints(String schemaName, String tableName, String columnName) {
        String sql = """
                SELECT rc.constraint_name, rc.table_name
                FROM information_schema.referential_constraints rc
                    JOIN information_schema.key_column_usage kcu
                        ON rc.constraint_name = kcu.constraint_name
                               AND rc.constraint_schema = kcu.constraint_schema
                WHERE rc.referenced_table_name = ?
                  AND rc.constraint_schema = ?
                  AND kcu.referenced_column_name = ?
                """;
        return jdbcTemplate.query(sql, (rs, rowNum) ->
                        rs.getString("constraint_name") + " ON " + rs.getString("table_name"),
                tableName, schemaName, columnName);
    }

    // Helper record for foreign key information
    private record ForeignKeyInfo(String referencedSchemaName, String referencedTableName,
                                  String referencedColumnName, String onUpdateAction, String onDeleteAction) {
    }
}
