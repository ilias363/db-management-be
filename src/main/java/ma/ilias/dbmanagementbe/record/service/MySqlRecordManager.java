package ma.ilias.dbmanagementbe.record.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.exception.ColumnNotFoundException;
import ma.ilias.dbmanagementbe.exception.InvalidRecordDataException;
import ma.ilias.dbmanagementbe.exception.RecordNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.ForeignKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykeyforeignkey.PrimaryKeyForeignKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.record.dto.*;
import ma.ilias.dbmanagementbe.util.SqlSecurityUtils;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
@Transactional
public class MySqlRecordManager implements RecordService {

    private final JdbcTemplate jdbcTemplate;
    private final MetadataProviderService metadataProviderService;

    @Override
    public RecordPageDto getRecords(String schemaName, String tableName, int page, int size,
                                    String sortBy, String sortDirection) {
        validateTableExists(schemaName, tableName);

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = schemaName.trim().toLowerCase();
        String validatedTableName = tableName.trim().toLowerCase();

        int offset = page * size;

        StringBuilder queryBuilder = new StringBuilder();
        queryBuilder.append("SELECT * FROM ").append(validatedSchemaName).append(".").append(validatedTableName);

        if (sortBy != null && !sortBy.isBlank()) {
            validateColumnExists(schemaName, tableName, sortBy);
            String validatedSortBy = SqlSecurityUtils.validateColumnName(sortBy);
            String validatedSortDirection = validateSortDirection(sortDirection);
            queryBuilder.append(" ORDER BY ").append(validatedSortBy).append(" ").append(validatedSortDirection);
        }

        // Add pagination
        queryBuilder.append(" LIMIT ? OFFSET ?");

        try {
            List<RecordDto> records = jdbcTemplate.query(
                    queryBuilder.toString(),
                    this::mapRowToRecord,
                    size, offset
            );

            long totalRecords = getRecordCount(validatedSchemaName, validatedTableName, false);
            int totalPages = (int) Math.ceil((double) totalRecords / size);

            return RecordPageDto.builder()
                    .records(records)
                    .totalRecords(totalRecords)
                    .currentPage(page)
                    .pageSize(size)
                    .totalPages(totalPages)
                    .tableName(validatedTableName)
                    .schemaName(validatedSchemaName)
                    .build();

        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to fetch records from table: " + e.getMessage(), e);
        }
    }

    @Override
    public RecordDto getRecord(String schemaName, String tableName, Map<String, Object> primaryKeyValues) {
        validateTableExists(schemaName, tableName);

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = schemaName.trim().toLowerCase();
        String validatedTableName = tableName.trim().toLowerCase();

        List<BaseColumnMetadataDto> primaryKeyColumns = getPrimaryKeyColumns(validatedSchemaName, validatedTableName);

        if (primaryKeyColumns.isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "Table has no primary key defined, use get by values");
        }

        // Build WHERE clause for primary key
        StringBuilder whereClause = new StringBuilder();
        List<Object> parameters = new ArrayList<>();

        for (int i = 0; i < primaryKeyColumns.size(); i++) {
            if (i > 0) whereClause.append(" AND ");

            String columnName = primaryKeyColumns.get(i).getColumnName();
            whereClause.append(columnName).append(" = ?");

            Object value = primaryKeyValues.get(columnName);
            if (value == null) {
                throw new InvalidRecordDataException(validatedTableName,
                        "Missing primary key value for column: " + columnName);
            }
            parameters.add(value);
        }

        String query = "SELECT * FROM " + validatedSchemaName + "." + validatedTableName +
                " WHERE " + whereClause;

        try {
            RecordDto record = jdbcTemplate.queryForObject(query, this::mapRowToRecord, parameters.toArray());
            if (record == null) {
                throw new RecordNotFoundException(validatedTableName, primaryKeyValues);
            }
            record.setSchemaName(validatedSchemaName);
            record.setTableName(validatedTableName);
            return record;
        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to fetch record: " + e.getMessage(), e);
        }
    }

    @Override
    public RecordDto createRecord(NewRecordDto newRecordDto) {
        validateTableExists(newRecordDto.getSchemaName(), newRecordDto.getTableName());

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = newRecordDto.getSchemaName().trim().toLowerCase();
        String validatedTableName = newRecordDto.getTableName().trim().toLowerCase();

        List<BaseColumnMetadataDto> columns = metadataProviderService.getColumnsByTable(
                validatedSchemaName, validatedTableName, false, false);

        validateRecordData(validatedSchemaName, validatedTableName, newRecordDto.getData(), columns, false);

        // Prepare column names and values
        List<String> columnNames = new ArrayList<>();
        List<Object> values = new ArrayList<>();
        List<String> placeholders = new ArrayList<>();

        for (BaseColumnMetadataDto column : columns) {
            String columnName = column.getColumnName();
            if (newRecordDto.getData().containsKey(columnName)) {
                columnNames.add(columnName);
                values.add(newRecordDto.getData().get(columnName));
                placeholders.add("?");
            } else if (!column.getIsNullable() && !column.getAutoIncrement()) {
                throw new InvalidRecordDataException(validatedTableName,
                        "Missing required value for non-nullable column: " + columnName);
            }
        }

        if (columnNames.isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "No valid column data provided");
        }

        String query = "INSERT INTO " + validatedSchemaName + "." + validatedTableName +
                " (" + String.join(", ", columnNames) + ") VALUES (" +
                String.join(", ", placeholders) + ")";

        try {
            jdbcTemplate.update(query, values.toArray());

            // For tables with auto-increment primary key, get the generated key
            List<BaseColumnMetadataDto> primaryKeyColumns = columns.stream()
                    .filter(column -> Set.of(ColumnType.PRIMARY_KEY, ColumnType.PRIMARY_KEY_FOREIGN_KEY)
                            .contains(column.getColumnType()))
                    .toList();

            if (primaryKeyColumns.isEmpty()) {
                Map<String, Object> identifyingValues = new HashMap<>();
                for (String columnName : columnNames) {
                    identifyingValues.put(columnName, newRecordDto.getData().get(columnName));
                }
                return getRecordByValues(validatedSchemaName, validatedTableName, identifyingValues);
            } else if (primaryKeyColumns.size() == 1 && primaryKeyColumns.get(0).getAutoIncrement()) {
                Long generatedId = jdbcTemplate.queryForObject("SELECT LAST_INSERT_ID()", Long.class);
                Map<String, Object> pkValues = new HashMap<>();
                pkValues.put(primaryKeyColumns.get(0).getColumnName(), generatedId);
                return getRecord(validatedSchemaName, validatedTableName, pkValues);
            } else {
                // Build primary key values from the inserted data
                Map<String, Object> pkValues = new HashMap<>();
                for (BaseColumnMetadataDto pkColumn : primaryKeyColumns) {
                    String columnName = pkColumn.getColumnName();
                    if (newRecordDto.getData().containsKey(columnName)) {
                        pkValues.put(columnName, newRecordDto.getData().get(columnName));
                    }
                }
                return getRecord(validatedSchemaName, validatedTableName, pkValues);
            }

        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to create record: " + e.getMessage(), e);
        }
    }

    @Override
    public RecordDto updateRecord(UpdateRecordDto updateRecordDto) {
        validateTableExists(updateRecordDto.getSchemaName(), updateRecordDto.getTableName());

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = updateRecordDto.getSchemaName().trim().toLowerCase();
        String validatedTableName = updateRecordDto.getTableName().trim().toLowerCase();

        // Check if table has primary key
        List<BaseColumnMetadataDto> primaryKeyColumns = getPrimaryKeyColumns(validatedSchemaName, validatedTableName);

        if (primaryKeyColumns.isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "Table has no primary key columns, use update by values");
        }

        // Verify the record exists for tables with primary keys
        getRecord(validatedSchemaName, validatedTableName, updateRecordDto.getPrimaryKeyValues());

        validateRecordData(validatedSchemaName, validatedTableName, updateRecordDto.getData(), null, true);

        List<String> setClauses = new ArrayList<>();
        List<Object> values = new ArrayList<>();

        for (Map.Entry<String, Object> entry : updateRecordDto.getData().entrySet()) {
            String columnName = SqlSecurityUtils.validateColumnName(entry.getKey());
            setClauses.add(columnName + " = ?");
            values.add(entry.getValue());
        }

        if (setClauses.isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "No data provided for update");
        }

        // Build WHERE clause for primary key
        List<String> whereClauses = new ArrayList<>();
        for (Map.Entry<String, Object> pkEntry : updateRecordDto.getPrimaryKeyValues().entrySet()) {
            // Pk column name is already validated when checking for the existence of the record
            String columnName = pkEntry.getKey();
            if (pkEntry.getValue() == null) {
                whereClauses.add(columnName + " IS NULL");
            } else {
                whereClauses.add(columnName + " = ?");
                values.add(pkEntry.getValue());
            }
        }

        String query = "UPDATE " + validatedSchemaName + "." + validatedTableName +
                " SET " + String.join(", ", setClauses) +
                " WHERE " + String.join(" AND ", whereClauses);

        try {
            int updatedRows = jdbcTemplate.update(query, values.toArray());

            if (updatedRows == 0) {
                throw new RecordNotFoundException(validatedTableName, updateRecordDto.getPrimaryKeyValues());
            }

            return getRecord(validatedSchemaName, validatedTableName, updateRecordDto.getPrimaryKeyValues());

        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to update record: " + e.getMessage(), e);
        }
    }

    @Override
    public boolean deleteRecord(String schemaName, String tableName, Map<String, Object> primaryKeyValues) {
        validateTableExists(schemaName, tableName);

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = schemaName.trim().toLowerCase();
        String validatedTableName = tableName.trim().toLowerCase();

        // Check if table has primary key
        List<BaseColumnMetadataDto> primaryKeyColumns = getPrimaryKeyColumns(validatedSchemaName, validatedTableName);

        if (primaryKeyColumns.isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "Table has no primary key columns, use delete by values");
        }

        if (primaryKeyValues == null || primaryKeyValues.isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "Primary key values cannot be null or empty");
        }

        // Verify the record exists
        getRecord(validatedSchemaName, validatedTableName, primaryKeyValues);

        List<String> whereClauses = new ArrayList<>();
        List<Object> values = new ArrayList<>();

        for (Map.Entry<String, Object> pkEntry : primaryKeyValues.entrySet()) {
            // Pk column name is already validated when checking for the existence of the record
            String columnName = pkEntry.getKey();
            if (pkEntry.getValue() == null) {
                whereClauses.add(columnName + " IS NULL");
            } else {
                whereClauses.add(columnName + " = ?");
                values.add(pkEntry.getValue());
            }
        }

        String query = "DELETE FROM " + validatedSchemaName + "." + validatedTableName +
                " WHERE " + String.join(" AND ", whereClauses);

        try {
            int deletedRows = jdbcTemplate.update(query, values.toArray());
            return deletedRows > 0;

        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to delete record: " + e.getMessage(), e);
        }
    }

    @Override
    public RecordDto getRecordByValues(String schemaName, String tableName, Map<String, Object> identifyingValues) {
        validateTableExists(schemaName, tableName);

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = schemaName.trim().toLowerCase();
        String validatedTableName = tableName.trim().toLowerCase();

        if (identifyingValues == null || identifyingValues.isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "Identifying values cannot be null or empty");
        }

        List<String> columnNames = metadataProviderService.getColumnsByTable(
                        validatedSchemaName,
                        validatedTableName,
                        false, false).stream()
                .map(BaseColumnMetadataDto::getColumnName)
                .toList();

        // Build WHERE clause using identifying values
        List<String> whereClauses = new ArrayList<>();
        List<Object> values = new ArrayList<>();

        for (Map.Entry<String, Object> entry : identifyingValues.entrySet()) {
            String columnName = SqlSecurityUtils.validateColumnName(entry.getKey());
            if (!columnNames.contains(columnName)) {
                throw new InvalidRecordDataException(tableName, "Column '" + columnName + "' does not exist");
            }
            if (entry.getValue() == null) {
                whereClauses.add(columnName + " IS NULL");
            } else {
                whereClauses.add(columnName + " = ?");
                values.add(entry.getValue());
            }
        }

        String query = "SELECT * FROM " + validatedSchemaName + "." + validatedTableName +
                " WHERE " + String.join(" AND ", whereClauses) + " LIMIT 1";

        try {
            RecordDto record = jdbcTemplate.queryForObject(query, this::mapRowToRecord, values.toArray());
            if (record == null) {
                throw new RecordNotFoundException("No record found matching the provided identifying values in table: " + validatedTableName);
            }
            record.setSchemaName(validatedSchemaName);
            record.setTableName(validatedTableName);
            return record;
        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to fetch record by values: " + e.getMessage(), e);
        }
    }

    @Override
    public RecordDto updateRecordByValues(UpdateRecordByValuesDto updateDto) {
        validateTableExists(updateDto.getSchemaName(), updateDto.getTableName());

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = updateDto.getSchemaName().trim().toLowerCase();
        String validatedTableName = updateDto.getTableName().trim().toLowerCase();

        // Verify the record exists for tables with identifying values
        getRecordByValues(validatedSchemaName, validatedTableName, updateDto.getIdentifyingValues());

        validateRecordData(validatedSchemaName, validatedTableName, updateDto.getNewData(), null, true);

        List<String> setClauses = new ArrayList<>();
        List<Object> values = new ArrayList<>();

        for (Map.Entry<String, Object> entry : updateDto.getNewData().entrySet()) {
            String columnName = SqlSecurityUtils.validateColumnName(entry.getKey());
            setClauses.add(columnName + " = ?");
            values.add(entry.getValue());
        }

        if (setClauses.isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "No data provided for update");
        }

        // Build WHERE clause using identifying values
        List<String> whereClauses = new ArrayList<>();
        for (Map.Entry<String, Object> entry : updateDto.getIdentifyingValues().entrySet()) {
            // Column name is already validated when checking for the existence of the record
            String columnName = entry.getKey();
            if (entry.getValue() == null) {
                whereClauses.add(columnName + " IS NULL");
            } else {
                whereClauses.add(columnName + " = ?");
                values.add(entry.getValue());
            }
        }

        String query = "UPDATE " + validatedSchemaName + "." + validatedTableName +
                " SET " + String.join(", ", setClauses) +
                " WHERE " + String.join(" AND ", whereClauses) +
                " LIMIT 1";

        try {
            int updatedRows = jdbcTemplate.update(query, values.toArray());

            if (updatedRows == 0) {
                throw new RecordNotFoundException("No record found matching the provided identifying values in table: " +
                        validatedTableName);
            }

            return getRecordByValues(validatedSchemaName, validatedTableName, updateDto.getIdentifyingValues());

        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to update record by values: " + e.getMessage(), e);
        }
    }

    @Override
    public long getRecordCount(String schemaName, String tableName, boolean checkTableExists) {
        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(schemaName);
        String validatedTableName = SqlSecurityUtils.validateTableName(tableName);

        if (checkTableExists) validateTableExists(validatedSchemaName, validatedTableName);

        String query = "SELECT COUNT(*) FROM " + validatedSchemaName + "." + validatedTableName;

        try {
            Long count = jdbcTemplate.queryForObject(query, Long.class);
            return count != null ? count : 0;
        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to get record count: " + e.getMessage(), e);
        }
    }

    @Override
    public void validateRecordData(String schemaName, String tableName, Map<String, Object> data,
                                   List<BaseColumnMetadataDto> columns, boolean isUpdate) {
        if (data == null || data.isEmpty()) {
            throw new InvalidRecordDataException(tableName, "Record data cannot be null or empty");
        }

        if (columns == null) {
            // columns are either provided from outside (already fetched),
            // or fetched here if 'columns=null'
            columns = metadataProviderService.getColumnsByTable(schemaName, tableName, false, false);
        }
        Map<String, BaseColumnMetadataDto> columnMap = columns.stream()
                .collect(Collectors.toMap(BaseColumnMetadataDto::getColumnName, col -> col));

        // Validate each provided column
        for (Map.Entry<String, Object> entry : data.entrySet()) {
            String columnName = entry.getKey();
            Object value = entry.getValue();

            BaseColumnMetadataDto column = columnMap.get(columnName);
            if (column == null) {
                throw new InvalidRecordDataException(tableName, "Column does not exist: " + columnName);
            }

            // Skip validation for auto-increment columns during insert
            if (!isUpdate && column.getAutoIncrement()) {
                continue;
            }

            // Validate null values
            if (value == null && !column.getIsNullable() && column.getColumnDefault() == null) {
                throw new InvalidRecordDataException(tableName,
                        "Cannot insert null value into non-nullable column: " + columnName);
            }

            if (column.getIsUnique()) {
                validateValueUniqueness(schemaName, tableName, columnName, value);
            }

            if (column instanceof ForeignKeyColumnMetadataDto fkCol) {
                validateFKValueExists(
                        fkCol.getReferencedSchemaName(),
                        fkCol.getReferencedTableName(),
                        fkCol.getReferencedColumnName(),
                        value, columnName);
            }
            if (column instanceof PrimaryKeyForeignKeyColumnMetadataDto pkFkCol) {
                validateFKValueExists(
                        pkFkCol.getReferencedSchemaName(),
                        pkFkCol.getReferencedTableName(),
                        pkFkCol.getReferencedColumnName(),
                        value, columnName);
            }

            validateValueDatatype(tableName, columnName, value, column);
        }
    }

    private void validateValueDatatype(String tableName, String columnName, Object value, BaseColumnMetadataDto column) {
        switch (column.getDataType().toUpperCase()) {
            case "VARCHAR", "CHAR" -> {
                if (value instanceof String stringValue) {
                    if (stringValue.length() > column.getCharacterMaxLength()) {
                        throw new InvalidRecordDataException(tableName,
                                "Value for column " + columnName + " exceeds max length of " + column.getCharacterMaxLength());
                    }
                } else {
                    throw new InvalidRecordDataException(tableName,
                            "Value for column " + columnName + " must be a string");
                }
            }
            case "TEXT" -> {
                if (!(value instanceof String)) {
                    throw new InvalidRecordDataException(tableName, "Value for column " + columnName + " must be a string");
                }
            }
            case "INT", "INTEGER", "SMALLINT", "BIGINT" -> {
                if (value instanceof String stringValue) {
                    if (!ValidationUtils.validateIntegerValue(stringValue)) {
                        throw new InvalidRecordDataException(tableName,
                                "Value for column " + columnName + " must be a valid integer");
                    }
                } else if (!(value instanceof Integer || value instanceof Long)) {
                    throw new InvalidRecordDataException(tableName, "Value for column " + columnName + " must be a integer");
                }
            }
            case "DECIMAL", "NUMERIC" -> {
                if (value instanceof Number numberValue) {
                    if (!ValidationUtils.validateDecimalFormat(
                            numberValue.toString(),
                            column.getNumericPrecision(),
                            column.getNumericScale())) {
                        throw new InvalidRecordDataException(tableName,
                                "Value for column " + columnName + " must be a valid decimal with precision " +
                                        column.getNumericPrecision() + " and scale " + column.getNumericScale());
                    }
                } else if (value instanceof String stringValue) {
                    if (!ValidationUtils.validateDecimalFormat(stringValue, column.getNumericPrecision(), column.getNumericScale())) {
                        throw new InvalidRecordDataException(tableName,
                                "Value for column " + columnName + " must be a valid decimal with precision " +
                                        column.getNumericPrecision() + " and scale " + column.getNumericScale());
                    }

                    if (!ValidationUtils.validateDecimalValue(stringValue)) {
                        throw new InvalidRecordDataException(tableName, "Value for column " + columnName + " must be a decimal");
                    }
                } else {
                    throw new InvalidRecordDataException(tableName, "Value for column " + columnName + " must be a decimal");
                }
            }
            case "FLOAT", "REAL", "DOUBLE" -> {
                if (value instanceof String stringValue) {
                    if (!ValidationUtils.validateFloatValue(stringValue)) {
                        throw new InvalidRecordDataException(tableName, "Value for column " + columnName + " must be a number");
                    }
                } else if (!(value instanceof Double)) {
                    throw new InvalidRecordDataException(tableName, "Value for column " + columnName + " must be a number");
                }
            }
            case "BOOLEAN" -> {
                if (!(value instanceof Boolean)) {
                    throw new InvalidRecordDataException(tableName, "Value for column " + columnName + " must be a boolean");
                }
            }
            case "DATE" -> {
                if (value instanceof String stringValue) {
                    if (!ValidationUtils.validateDateValue(stringValue)) {
                        throw new InvalidRecordDataException(tableName, "Value for column " + columnName +
                                " must be a valid date (yyyy-MM-dd)");
                    }
                } else {
                    throw new InvalidRecordDataException(tableName, "Value for column " + columnName +
                            " must be a string representing a date (yyyy-MM-dd");
                }
            }
            case "TIME" -> {
                if (value instanceof String stringValue) {
                    if (!ValidationUtils.validateTimeValue(stringValue)) {
                        throw new InvalidRecordDataException(tableName, "Value for column " + columnName +
                                " must be a valid time (HH:mm:ss)");
                    }
                } else {
                    throw new InvalidRecordDataException(tableName, "Value for column " + columnName +
                            " must be a string representing a time (HH:mm:ss)");
                }
            }
            case "TIMESTAMP" -> {
                if (value instanceof String stringValue) {
                    if (!ValidationUtils.validateTimestampValue(stringValue)) {
                        throw new InvalidRecordDataException(tableName, "Value for column " + columnName +
                                " must be a valid timestamp (yyyy-MM-dd HH:mm:ss)");
                    }
                } else {
                    throw new InvalidRecordDataException(tableName, "Value for column " + columnName +
                            " must be a string representing a timestamp (yyyy-MM-dd HH:mm:ss)");
                }
            }
            default -> throw new InvalidRecordDataException(tableName, "Invalid data type for column" + columnName);
        }
    }

    private void validateValueUniqueness(String schemaName, String tableName, String columnName, Object value) {
        Integer count;
        try {
            String checkSql = String.format(
                    "SELECT COUNT(*) FROM %s.%s WHERE %s = ?",
                    schemaName, tableName, columnName);

            count = jdbcTemplate.queryForObject(checkSql, Integer.class, value);
        } catch (Exception e) {
            throw new InvalidRecordDataException("Unable to validate column value uniqueness for " + columnName +
                    ": " + e.getMessage());
        }

        if (count != null && count > 0) {
            throw new InvalidRecordDataException(
                    String.format("Column value '%s' already exists in column %s", value, columnName));
        }
    }

    private void validateFKValueExists(String schemaName, String tableName, String columnName,
                                       Object value, String columnToValidateFor) {
        Integer count;
        try {
            String checkSql = String.format(
                    "SELECT COUNT(*) FROM %s.%s WHERE %s = ?",
                    schemaName, tableName, columnName);

            count = jdbcTemplate.queryForObject(checkSql, Integer.class, value);
        } catch (Exception e) {
            throw new InvalidRecordDataException("Unable to validate column '" + columnToValidateFor +
                    "' value existence in referenced table : " + e.getMessage());
        }

        if (count == null || count == 0) {
            throw new InvalidRecordDataException(
                    String.format("Column value '%s' does not exist in the referenced table '%s.%s' column '%s'",
                            value, schemaName, tableName, columnName));
        }
    }

    private List<BaseColumnMetadataDto> getPrimaryKeyColumns(String schemaName, String tableName) {
        return metadataProviderService.getColumnsByTable(schemaName, tableName, false, false)
                .stream()
                .filter(column -> Set.of(ColumnType.PRIMARY_KEY, ColumnType.PRIMARY_KEY_FOREIGN_KEY)
                        .contains(column.getColumnType()))
                .toList();
    }

    private RecordDto mapRowToRecord(ResultSet rs, int rowNum) throws SQLException {
        Map<String, Object> data = new HashMap<>();

        int columnCount = rs.getMetaData().getColumnCount();
        for (int i = 1; i <= columnCount; i++) {
            String columnName = rs.getMetaData().getColumnName(i);
            Object value = rs.getObject(i);
            data.put(columnName, value);
        }

        return RecordDto.builder()
                .data(data)
                .build();
    }

    private void validateTableExists(String schemaName, String tableName) {
        if (!metadataProviderService.tableExists(schemaName, tableName)) {
            throw new TableNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase());
        }
    }

    private void validateColumnExists(String schemaName, String tableName, String columnName) {
        if (!metadataProviderService.columnExists(schemaName, tableName, columnName)) {
            throw new ColumnNotFoundException(
                    schemaName.toLowerCase(),
                    tableName.toLowerCase(),
                    columnName.toLowerCase());
        }
    }

    private String validateSortDirection(String sortDirection) {
        if (sortDirection == null || sortDirection.isBlank()) {
            return "ASC";
        }
        String direction = sortDirection.trim().toUpperCase();
        if (!"ASC".equals(direction) && !"DESC".equals(direction)) {
            throw new IllegalArgumentException("Sort direction must be either ASC or DESC");
        }
        return direction;
    }
}
