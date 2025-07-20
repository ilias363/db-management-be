package ma.ilias.dbmanagementbe.metadata.service.column;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.ColumnNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.ForeignKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.NewPrimaryKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.PrimaryKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.NewStandardColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.StandardColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.*;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaService;
import ma.ilias.dbmanagementbe.metadata.service.table.TableService;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

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
                (rs, rowNum) -> rs.getString("COLUMN_NAME"));

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
                        rs.getString("UPDATE_RULE")));

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
                                        .build())
                        .build(),
                schemaName,
                tableName);

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
                    Boolean autoIncrement = rs.getString("EXTRA") != null
                            && rs.getString("EXTRA").toLowerCase().contains("auto_increment");
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
                columnName);
    }

    @Override
    public BaseColumnMetadataDto createColumn(BaseNewColumnDto newColumnDto) {
        if (newColumnDto instanceof NewPrimaryKeyColumnDto pkDto) {
            return createPrimaryKeyColumn(pkDto);
        }

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
        if (newColumnDto instanceof NewStandardColumnDto standardCol) {
            if (Boolean.FALSE.equals(standardCol.getIsNullable())) {
                alterSql.append(" NOT NULL");
            }

            if (Boolean.TRUE.equals(standardCol.getIsUnique())) {
                alterSql.append(" UNIQUE");
            }

            if (standardCol.getColumnDefault() != null) {
                if ("CURRENT_TIMESTAMP".equalsIgnoreCase(standardCol.getColumnDefault())) {
                    alterSql.append(" DEFAULT CURRENT_TIMESTAMP");
                } else if ("NULL".equalsIgnoreCase(standardCol.getColumnDefault())) {
                    alterSql.append(" DEFAULT NULL");
                } else {
                    alterSql.append(" DEFAULT '").append(standardCol.getColumnDefault()).append("'");
                }
            }
        } else if (newColumnDto instanceof NewPrimaryKeyColumnDto pkCol) {
            if (Boolean.TRUE.equals(pkCol.getAutoIncrement())) {
                alterSql.append(" AUTO_INCREMENT");
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
                    newColumnDto.getSchemaName(), newColumnDto.getTableName(), newColumnDto.getTableName(),
                    newColumnDto.getColumnName(),
                    newColumnDto.getColumnName(), fkDto.getReferencedSchemaName(),
                    fkDto.getReferencedTableName(), fkDto.getReferencedColumnName());

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
            throw new ColumnNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase(),
                    columnName.toLowerCase());
        }

        if (schemaService.isSystemSchemaByName(schemaName)) {
            throw new UnauthorizedActionException(
                    "Cannot delete column from system table: " + schemaName + "." + tableName);
        }

        String columnCountSql = """
                SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ?
                """;
        Integer columnCount = jdbcTemplate.queryForObject(columnCountSql, Integer.class, schemaName, tableName);
        if (columnCount != null && columnCount <= 1) {
            throw new UnauthorizedActionException(
                    "Table " + tableName + " has only one column. Try dropping the whole table instead.");
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
                columnName);

        if (!force && !fkConstraints.isEmpty()) {
            throw new UnauthorizedActionException(
                    "Cannot delete column used in a foreign key constraint. Use force=true to drop the constraint automatically.");
        }

        if (force && !fkConstraints.isEmpty()) {
            for (String fkConstraint : fkConstraints) {
                String sql = String.format("ALTER TABLE %s.%s DROP FOREIGN KEY %s", schemaName, tableName,
                        fkConstraint);
                jdbcTemplate.execute(sql);
            }
        }

        if (isPrimaryKey) {
            List<String> referencingFkConstraints = getReferencingForeignKeyConstraints(schemaName, tableName,
                    columnName);
            for (String fkConstraint : referencingFkConstraints) {
                String[] parts = fkConstraint.split(" ON ");
                String constraintName = parts[0];
                String referencingTableName = parts[1];
                String dropFkSql = String.format("ALTER TABLE %s.%s DROP FOREIGN KEY %s", schemaName,
                        referencingTableName, constraintName);
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
        return jdbcTemplate.query(sql,
                (rs, rowNum) -> rs.getString("constraint_name") + " ON " + rs.getString("table_name"),
                tableName, schemaName, columnName);
    }

    private BaseColumnMetadataDto createPrimaryKeyColumn(NewPrimaryKeyColumnDto pkDto) {
        String schemaName = pkDto.getSchemaName();
        String tableName = pkDto.getTableName();
        String columnName = pkDto.getColumnName();

        if (tableHasPrimaryKey(schemaName, tableName)) {
            throw new UnauthorizedActionException(
                    "Table " + schemaName + "." + tableName
                            + " already has a primary key. Drop the existing primary key first.");
        }

        StringBuilder alterSql = new StringBuilder("ALTER TABLE ")
                .append(schemaName)
                .append(".")
                .append(tableName)
                .append(" ADD COLUMN ")
                .append(columnName)
                .append(" ")
                .append(pkDto.getDataType());

        if (pkDto.getCharacterMaxLength() != null) {
            alterSql.append("(").append(pkDto.getCharacterMaxLength()).append(")");
        } else if (pkDto.getNumericPrecision() != null) {
            alterSql.append("(").append(pkDto.getNumericPrecision());
            if (pkDto.getNumericScale() != null) {
                alterSql.append(",").append(pkDto.getNumericScale());
            }
            alterSql.append(")");
        }

        // If auto-increment is used, existing rows will be automatically populated
        if (Boolean.TRUE.equals(pkDto.getAutoIncrement())) {
            alterSql.append(" AUTO_INCREMENT PRIMARY KEY");
            jdbcTemplate.execute(alterSql.toString());
            return getColumn(schemaName, tableName, columnName);
        }

        jdbcTemplate.execute(alterSql.toString());

        // Populate existing rows if the table has data and auto-increment is not used
        populatePrimaryKeyValues(pkDto);

        // Add primary key constraint
        String pkConstraintSql = String.format("ALTER TABLE %s.%s ADD PRIMARY KEY (%s)",
                schemaName, tableName, columnName);
        jdbcTemplate.execute(pkConstraintSql);

        return getColumn(schemaName, tableName, pkDto.getColumnName());
    }

    private boolean tableHasPrimaryKey(String schemaName, String tableName) {
        String sql = """
                SELECT COUNT(*) FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND CONSTRAINT_NAME = 'PRIMARY'
                """;
        Integer count = jdbcTemplate.queryForObject(sql, Integer.class, schemaName, tableName);
        return count != null && count > 0;
    }

    private void populatePrimaryKeyValues(NewPrimaryKeyColumnDto pkDto) {
        String schemaName = pkDto.getSchemaName();
        String tableName = pkDto.getTableName();
        String columnName = pkDto.getColumnName();
        String dataType = pkDto.getDataType().toUpperCase();

        // Check if table has data
        String countSql = String.format("SELECT COUNT(*) FROM %s.%s", schemaName, tableName);
        Integer rowCount = jdbcTemplate.queryForObject(countSql, Integer.class);

        if (rowCount == null || rowCount == 0) {
            return;
        }
        if (rowCount > 2 && dataType.equals("BOOLEAN")) {
            throw new UnauthorizedActionException(
                    "Cannot populate boolean primary key column that has more than 2 rows.");
        }

        switch (dataType) {
            case "INT":
            case "INTEGER":
            case "SMALLINT":
            case "BIGINT":
            case "DECIMAL":
            case "NUMERIC":
            case "FLOAT":
            case "DOUBLE":
            case "REAL":
                populateNumericPrimaryKey(schemaName, tableName, columnName);
                break;

            case "VARCHAR":
            case "CHAR":
            case "TEXT":
                populateStringPrimaryKey(schemaName, tableName, columnName);
                break;

            case "DATE":
                populateDatePrimaryKey(schemaName, tableName, columnName);
                break;

            case "TIME":
                populateTimePrimaryKey(schemaName, tableName, columnName);
                break;

            case "TIMESTAMP":
                populateTimestampPrimaryKey(schemaName, tableName, columnName);
                break;

            case "BOOLEAN":
                populateBooleanPrimaryKey(schemaName, tableName, columnName);
                break;

            default:
                break;
        }
    }

    private void populateNumericPrimaryKey(String schemaName, String tableName, String columnName) {
        String updateSql = String.format(
                "UPDATE %s.%s SET %s = (@row_number := @row_number + 1)",
                schemaName, tableName, columnName);

        jdbcTemplate.execute("SET @row_number = 0");
        jdbcTemplate.execute(updateSql);
    }

    private void populateStringPrimaryKey(String schemaName, String tableName, String columnName) {
        String updateSql = String.format(
                "UPDATE %s.%s SET %s = CAST((@row_number := @row_number + 1) AS CHAR)",
                schemaName, tableName, columnName);

        jdbcTemplate.execute("SET @row_number = 0");
        jdbcTemplate.execute(updateSql);
    }

    private void populateDatePrimaryKey(String schemaName, String tableName, String columnName) {
        String updateSql = String.format(
                "UPDATE %s.%s SET %s = DATE_ADD('2025-01-01', INTERVAL (@row_number := @row_number + 1) - 1 DAY)",
                schemaName, tableName, columnName);

        jdbcTemplate.execute("SET @row_number = 0");
        jdbcTemplate.execute(updateSql);
    }

    private void populateTimePrimaryKey(String schemaName, String tableName, String columnName) {
        String updateSql = String.format(
                "UPDATE %s.%s SET %s = SEC_TO_TIME((@row_number := @row_number + 1) * 60)",
                schemaName, tableName, columnName);

        jdbcTemplate.execute("SET @row_number = 0");
        jdbcTemplate.execute(updateSql);
    }

    private void populateTimestampPrimaryKey(String schemaName, String tableName, String columnName) {
        String updateSql = String.format(
                "UPDATE %s.%s SET %s = FROM_UNIXTIME(UNIX_TIMESTAMP('2025-01-01 00:00:00') + (@row_number := @row_number + 1) * 3600)",
                schemaName, tableName, columnName);

        jdbcTemplate.execute("SET @row_number = 0");
        jdbcTemplate.execute(updateSql);
    }

    private void populateBooleanPrimaryKey(String schemaName, String tableName, String columnName) {
        String updateSql = String.format(
                "UPDATE %s.%s SET %s = (@row_number := @row_number + 1) %% 2",
                schemaName, tableName, columnName);

        jdbcTemplate.execute("SET @row_number = 0");
        jdbcTemplate.execute(updateSql);
    }

    // Helper record for foreign key information
    private record ForeignKeyInfo(String referencedSchemaName, String referencedTableName,
            String referencedColumnName, String onUpdateAction, String onDeleteAction) {
    }

    @Override
    public BaseColumnMetadataDto renameColumn(RenameColumnDto renameColumnDto) {
        String sql = "ALTER TABLE " + renameColumnDto.getSchemaName() + "." + renameColumnDto.getTableName() +
                " RENAME COLUMN " + renameColumnDto.getColumnName() + " TO " + renameColumnDto.getNewColumnName();

        jdbcTemplate.execute(sql);

        return getColumn(renameColumnDto.getSchemaName(), renameColumnDto.getTableName(),
                renameColumnDto.getNewColumnName());
    }

    @Override
    public BaseColumnMetadataDto updateColumnDataType(UpdateColumnDataTypeDto updateColDataTypeDto) {
        StringBuilder dataTypeDefinition = new StringBuilder(updateColDataTypeDto.getDataType());

        if (updateColDataTypeDto.getCharacterMaxLength() != null) {
            dataTypeDefinition.append("(").append(updateColDataTypeDto.getCharacterMaxLength()).append(")");
        } else if (updateColDataTypeDto.getNumericPrecision() != null) {
            dataTypeDefinition.append("(").append(updateColDataTypeDto.getNumericPrecision());
            if (updateColDataTypeDto.getNumericScale() != null) {
                dataTypeDefinition.append(",").append(updateColDataTypeDto.getNumericScale());
            }
            dataTypeDefinition.append(")");
        }

        String sql = "ALTER TABLE " + updateColDataTypeDto.getSchemaName() + "." + updateColDataTypeDto.getTableName() +
                " MODIFY COLUMN " + updateColDataTypeDto.getColumnName() + " " + dataTypeDefinition;

        jdbcTemplate.execute(sql);

        return getColumn(updateColDataTypeDto.getSchemaName(),
                updateColDataTypeDto.getTableName(),
                updateColDataTypeDto.getColumnName());
    }

    @Override
    public BaseColumnMetadataDto updateColumnAutoIncrement(UpdateColumnAutoIncrementDto updateColAutoIncrementDto) {
        BaseColumnMetadataDto currentColumn = getColumn(updateColAutoIncrementDto.getSchemaName(),
                updateColAutoIncrementDto.getTableName(),
                updateColAutoIncrementDto.getColumnName());

        if (currentColumn.getAutoIncrement() == updateColAutoIncrementDto.getAutoIncrement()) {
            return currentColumn;
        }

        StringBuilder columnDefinition = new StringBuilder(currentColumn.getDataType());

        if (currentColumn.getCharacterMaxLength() != null) {
            columnDefinition.append("(").append(currentColumn.getCharacterMaxLength()).append(")");
        } else if (Set.of("NUMERIC", "DECIMAL").contains(currentColumn.getDataType().toUpperCase()) &&
                currentColumn.getNumericPrecision() != null) {
            columnDefinition.append("(").append(currentColumn.getNumericPrecision());
            if (currentColumn.getNumericScale() != null) {
                columnDefinition.append(",").append(currentColumn.getNumericScale());
            }
            columnDefinition.append(")");
        }

        if (updateColAutoIncrementDto.getAutoIncrement()) {
            columnDefinition.append(" AUTO_INCREMENT");
        }

        String sql = "ALTER TABLE " + updateColAutoIncrementDto.getSchemaName() + "."
                + updateColAutoIncrementDto.getTableName() +
                " MODIFY COLUMN " + updateColAutoIncrementDto.getColumnName() + " " + columnDefinition;

        jdbcTemplate.execute(sql);

        return getColumn(updateColAutoIncrementDto.getSchemaName(),
                updateColAutoIncrementDto.getTableName(),
                updateColAutoIncrementDto.getColumnName());
    }

    @Override
    public BaseColumnMetadataDto updateColumnNullable(UpdateColumnNullableDto updateColNullableDto, boolean populate) {
        BaseColumnMetadataDto currentColumn = getColumn(updateColNullableDto.getSchemaName(),
                updateColNullableDto.getTableName(),
                updateColNullableDto.getColumnName());

        if (currentColumn.getIsNullable() == updateColNullableDto.getIsNullable()) {
            return currentColumn;
        }

        Integer nullCount = getColumnNullCount(
                updateColNullableDto.getSchemaName(),
                updateColNullableDto.getTableName(),
                updateColNullableDto.getColumnName());

        if (!populate && !updateColNullableDto.getIsNullable() && nullCount > 0) {
            throw new UnauthorizedActionException(
                    "Column contains null values, cannot make make it not null. Use populate=true to fill null values and proceed.");
        }

        if (populate && !updateColNullableDto.getIsNullable() && nullCount > 0) {
            if (currentColumn.getIsUnique() != null && currentColumn.getIsUnique()) {
                throw new UnauthorizedActionException(
                        "Column is unique and contains null values, cannot make it not null or fill its null values.");
            }
            String nullUpdateSql = String.format("UPDATE %s.%s SET %s = ? WHERE %s IS NULL",
                    updateColNullableDto.getSchemaName(),
                    updateColNullableDto.getTableName(),
                    updateColNullableDto.getColumnName(),
                    updateColNullableDto.getColumnName());

            if (currentColumn.getColumnDefault() != null) {
                jdbcTemplate.update(nullUpdateSql, currentColumn.getColumnDefault());
            } else {
                jdbcTemplate.update(nullUpdateSql, getDefaultForDataType(currentColumn.getDataType()));
            }
        }

        StringBuilder columnDefinition = new StringBuilder(currentColumn.getDataType());

        if (currentColumn.getCharacterMaxLength() != null) {
            columnDefinition.append("(").append(currentColumn.getCharacterMaxLength()).append(")");
        } else if (Set.of("NUMERIC", "DECIMAL").contains(currentColumn.getDataType().toUpperCase()) &&
                currentColumn.getNumericPrecision() != null) {
            columnDefinition.append("(").append(currentColumn.getNumericPrecision());
            if (currentColumn.getNumericScale() != null) {
                columnDefinition.append(",").append(currentColumn.getNumericScale());
            }
            columnDefinition.append(")");
        }

        if (updateColNullableDto.getIsNullable()) {
            columnDefinition.append(" NULL");
        } else {
            columnDefinition.append(" NOT NULL");
        }

        String sql = "ALTER TABLE " + updateColNullableDto.getSchemaName() + "." + updateColNullableDto.getTableName() +
                " MODIFY COLUMN " + updateColNullableDto.getColumnName() + " " + columnDefinition;

        jdbcTemplate.execute(sql);

        return getColumn(updateColNullableDto.getSchemaName(), updateColNullableDto.getTableName(),
                updateColNullableDto.getColumnName());
    }

    private Integer getColumnNullCount(String schemaName, String tableName, String columnName) {
        String sql = String.format("SELECT COUNT(*) FROM %s.%s WHERE %s IS NULL", schemaName, tableName, columnName);
        Integer count = jdbcTemplate.queryForObject(sql, Integer.class);
        return count == null ? 0 : count;
    }

    private String getDefaultForDataType(String dataType) {
        dataType = dataType.toUpperCase();
        if (dataType.contains("INT") || dataType.contains("DECIMAL") || dataType.contains("NUMERIC") ||
                dataType.contains("FLOAT") || dataType.contains("DOUBLE") || dataType.contains("REAL")) {
            return "0";
        }
        if (dataType.contains("CHAR") || dataType.contains("TEXT")) {
            return "'unknown'";
        }
        if (dataType.contains("DATE") && !dataType.contains("TIMESTAMP")) {
            return "'1970-01-01'";
        }
        if (dataType.contains("TIME")) {
            return "'00:00:00'";
        }
        if (dataType.contains("BOOL")) {
            return "FALSE";
        }
        if (dataType.contains("BINARY") || dataType.contains("BLOB")) {
            return "''";
        }
        throw new IllegalArgumentException("Unsupported data type: " + dataType);
    }

    @Override
    public BaseColumnMetadataDto updateColumnUnique(UpdateColumnUniqueDto updateColUniqueDto) {
        String uniqueColConstraintsSql = """
                SELECT c.CONSTRAINT_NAME
                FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE c
                JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS t
                  ON c.CONSTRAINT_NAME = t.CONSTRAINT_NAME
                WHERE c.TABLE_SCHEMA = ? AND c.TABLE_NAME = ? AND c.COLUMN_NAME = ?
                  AND t.CONSTRAINT_TYPE = 'UNIQUE'
                """;

        List<String> uniqueConstraints = jdbcTemplate.query(
                uniqueColConstraintsSql,
                ps -> {
                    ps.setString(1, updateColUniqueDto.getSchemaName());
                    ps.setString(2, updateColUniqueDto.getTableName());
                    ps.setString(3, updateColUniqueDto.getColumnName());
                },
                (rs, rowNum) -> rs.getString("CONSTRAINT_NAME"));

        if (updateColUniqueDto.getIsUnique() == !uniqueConstraints.isEmpty()) {
            return getColumn(updateColUniqueDto.getSchemaName(),
                    updateColUniqueDto.getTableName(),
                    updateColUniqueDto.getColumnName());
        }

        if (updateColUniqueDto.getIsUnique()) {
            if (columnHasDuplicates(
                    updateColUniqueDto.getSchemaName(),
                    updateColUniqueDto.getTableName(),
                    updateColUniqueDto.getColumnName())) {
                throw new UnauthorizedActionException(
                        "Cannot add unique constraint: column contains duplicate values.");
            }
        }

        String sql;
        if (updateColUniqueDto.getIsUnique()) {
            sql = String.format("ALTER TABLE %s.%s ADD CONSTRAINT UNIQUE (%s)",
                    updateColUniqueDto.getSchemaName(),
                    updateColUniqueDto.getTableName(),
                    updateColUniqueDto.getColumnName());
        } else {
            sql = String.format("ALTER TABLE %s.%s DROP CONSTRAINT %s",
                    updateColUniqueDto.getSchemaName(),
                    updateColUniqueDto.getTableName(),
                    uniqueConstraints.get(0));
        }

        jdbcTemplate.execute(sql);
        return getColumn(updateColUniqueDto.getSchemaName(),
                updateColUniqueDto.getTableName(),
                updateColUniqueDto.getColumnName());
    }

    private boolean columnHasDuplicates(String schemaName, String tableName, String columnName) {
        String duplicateCheckSql = String.format(
                "SELECT %s FROM %s.%s GROUP BY %s HAVING COUNT(*) > 1 LIMIT 1",
                columnName, schemaName, tableName, columnName);

        List<Object> duplicates = jdbcTemplate.query(
                duplicateCheckSql,
                (rs, rowNum) -> rs.getObject(columnName));
        return !duplicates.isEmpty();
    }

    @Override
    public BaseColumnMetadataDto updateColumnDefault(UpdateColumnDefaultDto updateColDefaultDto) {
        BaseColumnMetadataDto currentColumn = getColumn(
                updateColDefaultDto.getSchemaName(),
                updateColDefaultDto.getTableName(),
                updateColDefaultDto.getColumnName());

        StringBuilder columnDefinition = new StringBuilder(currentColumn.getDataType());

        if (currentColumn.getCharacterMaxLength() != null) {
            columnDefinition.append("(").append(currentColumn.getCharacterMaxLength()).append(")");
        } else if (Set.of("NUMERIC", "DECIMAL").contains(currentColumn.getDataType().toUpperCase()) &&
                currentColumn.getNumericPrecision() != null) {
            columnDefinition.append("(").append(currentColumn.getNumericPrecision());
            if (currentColumn.getNumericScale() != null) {
                columnDefinition.append(",").append(currentColumn.getNumericScale());
            }
            columnDefinition.append(")");
        }

        if (currentColumn.getIsNullable() != null && !currentColumn.getIsNullable()) {
            columnDefinition.append(" NOT NULL");
        }

        if (updateColDefaultDto.getColumnDefault() != null) {
            if ("CURRENT_TIMESTAMP".equalsIgnoreCase(updateColDefaultDto.getColumnDefault())) {
                columnDefinition.append(" DEFAULT CURRENT_TIMESTAMP");
            } else if ("NULL".equalsIgnoreCase(updateColDefaultDto.getColumnDefault())) {
                columnDefinition.append(" DEFAULT NULL");
            } else {
                columnDefinition.append(" DEFAULT '").append(updateColDefaultDto.getColumnDefault()).append("'");
            }
        }

        String sql = String.format("ALTER TABLE %s.%s MODIFY COLUMN %s %s",
                updateColDefaultDto.getSchemaName(),
                updateColDefaultDto.getTableName(),
                updateColDefaultDto.getColumnName(),
                columnDefinition);

        jdbcTemplate.execute(sql);

        return getColumn(updateColDefaultDto.getSchemaName(), updateColDefaultDto.getTableName(),
                updateColDefaultDto.getColumnName());
    }

    @Override
    public List<BaseColumnMetadataDto> updateColumnPrimaryKey(UpdateColumnPrimaryKeyDto updateColPKDto, boolean force) {
        boolean isCurrentlyPrimaryKey = updateColPKDto.getColumnNames().stream().anyMatch(
                colName -> isColumnPrimaryKey(updateColPKDto.getSchemaName(), updateColPKDto.getTableName(), colName));

        List<String> currentPkColumns = getCurrentPrimaryKeyColumns(
                updateColPKDto.getSchemaName(),
                updateColPKDto.getTableName());

        if (updateColPKDto.getIsPrimaryKey()) {
            if (!currentPkColumns.isEmpty()) {
                throw new UnauthorizedActionException(
                        "Table already has a primary key. Drop the existing primary key first.");
            }

            if (!isCurrentlyPrimaryKey) {
                if (compositeKeyHasDuplicates(
                        updateColPKDto.getSchemaName(),
                        updateColPKDto.getTableName(),
                        currentPkColumns)) {
                    throw new UnauthorizedActionException(
                            "Cannot add primary key: the column combination would create duplicate values.");
                }

                // Add the new composite primary key
                String addPkSql = String.format("ALTER TABLE %s.%s ADD PRIMARY KEY (%s)",
                        updateColPKDto.getSchemaName(),
                        updateColPKDto.getTableName(),
                        String.join(", ", currentPkColumns));
                jdbcTemplate.execute(addPkSql);
            }
        } else {
            List<String> referencingFkConstraints = new ArrayList<>();
            currentPkColumns.forEach(colName -> {
                List<String> constraints = getReferencingForeignKeyConstraints(
                        updateColPKDto.getSchemaName(),
                        updateColPKDto.getTableName(),
                        colName);

                referencingFkConstraints.addAll(constraints);
            });

            if (!referencingFkConstraints.isEmpty() && !force) {
                throw new UnauthorizedActionException(
                        "Cannot drop primary key constraint: " + referencingFkConstraints.size() +
                                " foreign key constraints reference this primary key. Use force=true to proceed.");
            }

            if (!referencingFkConstraints.isEmpty()) {
                for (String fkConstraint : referencingFkConstraints) {
                    String[] parts = fkConstraint.split(" ON ");
                    String constraintName = parts[0];
                    String referencingTableName = parts[1];
                    String dropFkSql = String.format("ALTER TABLE %s.%s DROP FOREIGN KEY %s",
                            updateColPKDto.getSchemaName(), referencingTableName, constraintName);
                    jdbcTemplate.execute(dropFkSql);
                }
            }

            // Remove auto increment if present
            currentPkColumns.forEach(colName -> {
                UpdateColumnAutoIncrementDto updateAIDto = new UpdateColumnAutoIncrementDto();
                updateAIDto.setSchemaName(updateColPKDto.getSchemaName());
                updateAIDto.setTableName(updateColPKDto.getTableName());
                updateAIDto.setColumnName(colName);
                updateAIDto.setAutoIncrement(false);
                updateColumnAutoIncrement(updateAIDto);
            });

            String sql = String.format("ALTER TABLE %s.%s DROP PRIMARY KEY",
                    updateColPKDto.getSchemaName(),
                    updateColPKDto.getTableName());
            jdbcTemplate.execute(sql);
        }

        return updateColPKDto.getColumnNames().stream()
                .map(colName -> getColumn(updateColPKDto.getSchemaName(), updateColPKDto.getTableName(), colName))
                .collect(Collectors.toList());
    }

    @Override
    public BaseColumnMetadataDto updateColumnForeignKey(UpdateColumnForeignKeyDto updateColFKDto) {
        if (updateColFKDto.getIsForeignKey()) {
            validateForeignKeyValuesExist(updateColFKDto);

            StringBuilder fkSql = new StringBuilder(String.format(
                    "ALTER TABLE %s.%s ADD CONSTRAINT FOREIGN KEY (%s) REFERENCES %s.%s(%s)",
                    updateColFKDto.getSchemaName(),
                    updateColFKDto.getTableName(),
                    updateColFKDto.getColumnName(),
                    updateColFKDto.getReferencedSchemaName(),
                    updateColFKDto.getReferencedTableName(),
                    updateColFKDto.getReferencedColumnName()));

            if (updateColFKDto.getOnUpdateAction() != null && !updateColFKDto.getOnUpdateAction().isBlank()) {
                fkSql.append(" ON UPDATE ").append(updateColFKDto.getOnUpdateAction());
            }

            if (updateColFKDto.getOnDeleteAction() != null && !updateColFKDto.getOnDeleteAction().isBlank()) {
                fkSql.append(" ON DELETE ").append(updateColFKDto.getOnDeleteAction());
            }

            jdbcTemplate.execute(fkSql.toString());
        } else {
            String fkConstraintName = getForeignKeyConstraintName(
                    updateColFKDto.getSchemaName(),
                    updateColFKDto.getTableName(),
                    updateColFKDto.getColumnName());

            if (fkConstraintName == null) {
                throw new UnauthorizedActionException("Column is not a foreign key");
            }

            String sql = String.format("ALTER TABLE %s.%s DROP FOREIGN KEY %s",
                    updateColFKDto.getSchemaName(),
                    updateColFKDto.getTableName(),
                    fkConstraintName);

            jdbcTemplate.execute(sql);
        }

        return getColumn(updateColFKDto.getSchemaName(),
                updateColFKDto.getTableName(),
                updateColFKDto.getColumnName());
    }

    private String getForeignKeyConstraintName(String schemaName, String tableName, String columnName) {
        String sql = """
                SELECT CONSTRAINT_NAME
                FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND COLUMN_NAME = ?
                  AND REFERENCED_TABLE_NAME IS NOT NULL
                """;

        List<String> constraints = jdbcTemplate.query(
                sql,
                (rs, rowNum) -> rs.getString("CONSTRAINT_NAME"),
                schemaName,
                tableName,
                columnName);

        return constraints.isEmpty() ? null : constraints.get(0);
    }

    private void validateForeignKeyValuesExist(UpdateColumnForeignKeyDto dto) {
        String sql = String.format(
                "SELECT COUNT(*) FROM %s.%s fk LEFT JOIN %s.%s ref ON fk.%s = ref.%s WHERE fk.%s IS NOT NULL AND ref.%s IS NULL",
                dto.getSchemaName(), dto.getTableName(),
                dto.getReferencedSchemaName(), dto.getReferencedTableName(),
                dto.getColumnName(), dto.getReferencedColumnName(),
                dto.getColumnName(), dto.getReferencedColumnName());
        Integer count = jdbcTemplate.queryForObject(sql, Integer.class);
        if (count != null && count > 0) {
            throw new UnauthorizedActionException(
                    "Foreign key constraint violation: " + count + " value(s) in column '" + dto.getColumnName() +
                            "' do not exist in referenced column '" + dto.getReferencedColumnName() + "'.");
        }
    }

    private List<String> getCurrentPrimaryKeyColumns(String schemaName, String tableName) {
        String sql = """
                SELECT COLUMN_NAME
                FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND CONSTRAINT_NAME = 'PRIMARY'
                ORDER BY ORDINAL_POSITION
                """;

        return jdbcTemplate.query(
                sql,
                (rs, rowNum) -> rs.getString("COLUMN_NAME"),
                schemaName,
                tableName);
    }

    private boolean compositeKeyHasDuplicates(String schemaName, String tableName, List<String> columnNames) {
        if (columnNames.isEmpty()) {
            return false;
        }

        String columnsStr = String.join(", ", columnNames);
        String duplicateCheckSql = String.format(
                "SELECT %s FROM %s.%s GROUP BY %s HAVING COUNT(*) > 1 LIMIT 1",
                columnsStr, schemaName, tableName, columnsStr);

        List<Object> duplicates = jdbcTemplate.query(
                duplicateCheckSql,
                (rs, rowNum) -> rs.getObject(1));
        return !duplicates.isEmpty();
    }
}