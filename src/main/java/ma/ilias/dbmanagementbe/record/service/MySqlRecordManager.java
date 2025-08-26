package ma.ilias.dbmanagementbe.record.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.exception.*;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseTableColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.ForeignKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykeyforeignkey.PrimaryKeyForeignKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.view.ViewColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.record.dto.*;
import ma.ilias.dbmanagementbe.record.utils.SearchQueryBuilder;
import ma.ilias.dbmanagementbe.service.DatabaseAuthorizationService;
import ma.ilias.dbmanagementbe.util.SqlSecurityUtils;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.BatchPreparedStatementSetter;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.*;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
@Transactional
public class MySqlRecordManager implements RecordService {

    private final JdbcTemplate jdbcTemplate;
    private final MetadataProviderService metadataProviderService;
    private final DatabaseAuthorizationService databaseAuthorizationService;

    @Override
    public RecordPageDto getRecords(String schemaName, String tableName, int page, int size,
                                    String sortBy, String sortDirection) {
        databaseAuthorizationService.checkReadPermission(schemaName, tableName);

        validateTableExists(schemaName, tableName);

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = schemaName.trim().toLowerCase();
        String validatedTableName = tableName.trim().toLowerCase();

        int offset = page * size;

        StringBuilder queryBuilder = new StringBuilder();
        queryBuilder.append("SELECT * FROM ").append(validatedSchemaName).append(".").append(validatedTableName);

        if (sortBy != null && !sortBy.isBlank()) {
            String validatedSortBy = SqlSecurityUtils.validateColumnName(sortBy);
            validateColumnExists(validatedSchemaName, validatedTableName, validatedSortBy);
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
                    .items(records)
                    .totalItems(totalRecords)
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
    public ViewRecordPageDto getViewRecords(String schemaName, String viewName, int page, int size,
                                            String sortBy, String sortDirection) {
        databaseAuthorizationService.checkReadPermission(schemaName, viewName);

        validateViewExists(schemaName, viewName);

        // schema name and view name are validated during the view existence check
        String validatedSchemaName = schemaName.trim().toLowerCase();
        String validatedViewName = viewName.trim().toLowerCase();

        int offset = page * size;

        StringBuilder queryBuilder = new StringBuilder();
        queryBuilder.append("SELECT * FROM ").append(validatedSchemaName).append(".").append(validatedViewName);

        if (sortBy != null && !sortBy.isBlank()) {
            String validatedSortBy = SqlSecurityUtils.validateColumnName(sortBy);
            validateViewColumnExists(validatedSchemaName, validatedViewName, validatedSortBy);
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

            long totalRecords = getViewRecordCount(validatedSchemaName, validatedViewName, false, false);
            int totalPages = (int) Math.ceil((double) totalRecords / size);

            return ViewRecordPageDto.builder()
                    .items(records)
                    .totalItems(totalRecords)
                    .currentPage(page)
                    .pageSize(size)
                    .totalPages(totalPages)
                    .viewName(validatedViewName)
                    .schemaName(validatedSchemaName)
                    .build();

        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to fetch records from view: " + e.getMessage(), e);
        }
    }

    @Override
    public RecordDto getRecord(String schemaName, String tableName, Map<String, Object> primaryKeyValues) {
        return getRecord(schemaName, tableName, primaryKeyValues, false);
    }

    @Override
    public RecordDto getRecord(String schemaName, String tableName, Map<String, Object> primaryKeyValues,
                               boolean checkAuthorization) {
        if (checkAuthorization) databaseAuthorizationService.checkReadPermission(schemaName, tableName);

        validateTableExists(schemaName, tableName);

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = schemaName.trim().toLowerCase();
        String validatedTableName = tableName.trim().toLowerCase();

        List<BaseTableColumnMetadataDto> primaryKeyColumns = getPrimaryKeyColumns(validatedSchemaName, validatedTableName);

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
        databaseAuthorizationService.checkCreatePermission(newRecordDto.getSchemaName(), newRecordDto.getTableName());

        validateTableExists(newRecordDto.getSchemaName(), newRecordDto.getTableName());

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = newRecordDto.getSchemaName().trim().toLowerCase();
        String validatedTableName = newRecordDto.getTableName().trim().toLowerCase();

        List<BaseTableColumnMetadataDto> columns = metadataProviderService.getColumnsByTable(
                validatedSchemaName, validatedTableName, false, false);

        validateRecordData(validatedSchemaName, validatedTableName, newRecordDto.getData(), columns, false);

        // Prepare column names and values
        List<String> columnNames = new ArrayList<>();
        List<Object> values = new ArrayList<>();
        List<String> placeholders = new ArrayList<>();

        for (BaseTableColumnMetadataDto column : columns) {
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
            KeyHolder keyHolder = new GeneratedKeyHolder();

            jdbcTemplate.update(connection -> {
                PreparedStatement ps = connection.prepareStatement(query, Statement.RETURN_GENERATED_KEYS);
                for (int i = 0; i < values.size(); i++) {
                    ps.setObject(i + 1, values.get(i));
                }
                return ps;
            }, keyHolder);

            // For tables with auto-increment primary key, get the generated key
            List<BaseTableColumnMetadataDto> primaryKeyColumns = columns.stream()
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
                Number generatedId = keyHolder.getKey();
                if (generatedId == null) {
                    throw new RuntimeException("Failed to retrieve generated key");
                }
                Map<String, Object> pkValues = new HashMap<>();
                pkValues.put(primaryKeyColumns.get(0).getColumnName(), generatedId.longValue());
                return getRecord(validatedSchemaName, validatedTableName, pkValues);
            } else {
                // Build primary key values from the inserted data
                Map<String, Object> pkValues = new HashMap<>();
                for (BaseTableColumnMetadataDto pkColumn : primaryKeyColumns) {
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
        databaseAuthorizationService.checkWritePermission(updateRecordDto.getSchemaName(), updateRecordDto.getTableName());

        validateTableExists(updateRecordDto.getSchemaName(), updateRecordDto.getTableName());

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = updateRecordDto.getSchemaName().trim().toLowerCase();
        String validatedTableName = updateRecordDto.getTableName().trim().toLowerCase();

        List<BaseTableColumnMetadataDto> primaryKeyColumns = getPrimaryKeyColumns(validatedSchemaName, validatedTableName);

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

            return getRecord(validatedSchemaName, validatedTableName, updateRecordDto.getData());

        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to update record: " + e.getMessage(), e);
        }
    }

    @Override
    public boolean deleteRecord(DeleteRecordDto deleteRecordDto) {
        databaseAuthorizationService.checkDeletePermission(deleteRecordDto.getSchemaName(), deleteRecordDto.getTableName());

        validateTableExists(deleteRecordDto.getSchemaName(), deleteRecordDto.getTableName());

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = deleteRecordDto.getSchemaName().trim().toLowerCase();
        String validatedTableName = deleteRecordDto.getTableName().trim().toLowerCase();

        List<BaseTableColumnMetadataDto> primaryKeyColumns = getPrimaryKeyColumns(validatedSchemaName, validatedTableName);

        if (primaryKeyColumns.isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "Table has no primary key columns, use delete by values");
        }

        if (deleteRecordDto.getPrimaryKeyValues() == null || deleteRecordDto.getPrimaryKeyValues().isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "Primary key values cannot be null or empty");
        }

        // Validate that all required primary key columns are provided
        Set<String> providedPkColumns = deleteRecordDto.getPrimaryKeyValues().keySet().stream()
                .map(String::trim)
                .map(String::toLowerCase)
                .collect(Collectors.toSet());
        Set<String> requiredPkColumns = primaryKeyColumns.stream()
                .map(BaseTableColumnMetadataDto::getColumnName)
                .map(String::toLowerCase)
                .collect(Collectors.toSet());

        if (!providedPkColumns.containsAll(requiredPkColumns)) {
            Set<String> missingColumns = new HashSet<>(requiredPkColumns);
            missingColumns.removeAll(providedPkColumns);
            throw new InvalidRecordDataException(validatedTableName,
                    "Missing required primary key columns: " + String.join(", ", missingColumns));
        }

        if (!requiredPkColumns.containsAll(providedPkColumns)) {
            Set<String> invalidColumns = new HashSet<>(providedPkColumns);
            invalidColumns.removeAll(requiredPkColumns);
            throw new InvalidRecordDataException(validatedTableName,
                    "Invalid primary key columns provided: " + String.join(", ", invalidColumns));
        }

        List<String> whereClauses = new ArrayList<>();
        List<Object> values = new ArrayList<>();

        for (Map.Entry<String, Object> pkEntry : deleteRecordDto.getPrimaryKeyValues().entrySet()) {
            String columnName = SqlSecurityUtils.validateColumnName(pkEntry.getKey());
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
        return getRecordByValues(schemaName, tableName, identifyingValues, false);
    }

    @Override
    public RecordDto getRecordByValues(String schemaName, String tableName, Map<String, Object> identifyingValues,
                                       boolean checkAuthorization) {
        if (checkAuthorization) databaseAuthorizationService.checkReadPermission(schemaName, tableName);

        List<RecordDto> records = getRecordsByValues(schemaName, tableName, identifyingValues, true);
        if (records.isEmpty()) {
            throw new RecordNotFoundException("No record found matching the provided identifying values in table: " + tableName);
        }
        return records.get(0);
    }

    private List<RecordDto> getRecordsByValues(String schemaName, String tableName, Map<String, Object> identifyingValues,
                                               boolean limitOne) {

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
                .map(BaseTableColumnMetadataDto::getColumnName)
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
                " WHERE " + String.join(" AND ", whereClauses);

        if (limitOne) {
            query += " LIMIT 1";
        }

        try {
            List<RecordDto> records = jdbcTemplate.query(query, this::mapRowToRecord, values.toArray());

            for (RecordDto record : records) {
                record.setSchemaName(validatedSchemaName);
                record.setTableName(validatedTableName);
            }

            return records;
        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to fetch records by values: " + e.getMessage(), e);
        }
    }

    @Override
    public RecordPageDto getRecordsByValues(String schemaName, String tableName, Map<String, Object> identifyingValues,
                                            int page, int size, String sortBy, String sortDirection) {
        databaseAuthorizationService.checkReadPermission(schemaName, tableName);

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
                .map(BaseTableColumnMetadataDto::getColumnName)
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

        StringBuilder queryBuilder = new StringBuilder();
        queryBuilder.append("SELECT * FROM ").append(validatedSchemaName).append(".").append(validatedTableName)
                .append(" WHERE ").append(String.join(" AND ", whereClauses));

        // Add sorting if specified
        if (sortBy != null && !sortBy.isBlank()) {
            String validatedSortBy = SqlSecurityUtils.validateColumnName(sortBy);
            validateColumnExists(validatedSchemaName, validatedTableName, validatedSortBy);
            String validatedSortDirection = validateSortDirection(sortDirection);
            queryBuilder.append(" ORDER BY ").append(validatedSortBy).append(" ").append(validatedSortDirection);
        }

        int offset = page * size;

        queryBuilder.append(" LIMIT ? OFFSET ?");

        // Build a separate list for pagination parameters
        List<Object> paginatedValues = new ArrayList<>(values);
        paginatedValues.add(size);
        paginatedValues.add(offset);

        try {
            // Execute paginated query
            List<RecordDto> records = jdbcTemplate.query(
                    queryBuilder.toString(),
                    this::mapRowToRecord,
                    paginatedValues.toArray()
            );

            // Get total count for this filtered result set
            String countQuery = "SELECT COUNT(*) FROM " + validatedSchemaName + "." + validatedTableName +
                    " WHERE " + String.join(" AND ", whereClauses);

            Long totalRecords = jdbcTemplate.queryForObject(countQuery, Long.class, values.toArray());
            if (totalRecords == null) {
                totalRecords = 0L;
            }
            int totalPages = (int) Math.ceil((double) totalRecords / size);

            return RecordPageDto.builder()
                    .items(records)
                    .totalItems(totalRecords)
                    .currentPage(page)
                    .pageSize(size)
                    .totalPages(totalPages)
                    .tableName(validatedTableName)
                    .schemaName(validatedSchemaName)
                    .build();

        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to fetch records by values: " + e.getMessage(), e);
        }
    }

    @Override
    public ViewRecordPageDto getViewRecordsByValues(String schemaName, String viewName, Map<String, Object> identifyingValues,
                                                    int page, int size, String sortBy, String sortDirection) {
        databaseAuthorizationService.checkReadPermission(schemaName, viewName);
        validateViewExists(schemaName, viewName);

        String validatedSchemaName = schemaName.trim().toLowerCase();
        String validatedViewName = viewName.trim().toLowerCase();

        if (identifyingValues == null || identifyingValues.isEmpty()) {
            throw new InvalidRecordDataException(validatedViewName, "Identifying values cannot be null or empty");
        }

        List<String> columnNames = metadataProviderService.getColumnsByView(
                        validatedSchemaName, validatedViewName, false, false)
                .stream().map(ViewColumnMetadataDto::getColumnName)
                .toList();

        // Build WHERE clause
        List<String> whereClauses = new ArrayList<>();
        List<Object> values = new ArrayList<>();

        for (Map.Entry<String, Object> entry : identifyingValues.entrySet()) {
            String columnName = SqlSecurityUtils.validateColumnName(entry.getKey());
            if (!columnNames.contains(columnName)) {
                throw new InvalidRecordDataException(viewName, "Column '" + columnName + "' does not exist");
            }
            if (entry.getValue() == null) {
                whereClauses.add(columnName + " IS NULL");
            } else {
                whereClauses.add(columnName + " = ?");
                values.add(entry.getValue());
            }
        }

        StringBuilder queryBuilder = new StringBuilder();
        queryBuilder.append("SELECT * FROM ").append(validatedSchemaName).append(".").append(validatedViewName)
                .append(" WHERE ").append(String.join(" AND ", whereClauses));

        if (sortBy != null && !sortBy.isBlank()) {
            String validatedSortBy = SqlSecurityUtils.validateColumnName(sortBy);
            validateViewColumnExists(validatedSchemaName, validatedViewName, validatedSortBy);
            String validatedSortDirection = validateSortDirection(sortDirection);
            queryBuilder.append(" ORDER BY ").append(validatedSortBy).append(" ").append(validatedSortDirection);
        }

        int offset = page * size;
        queryBuilder.append(" LIMIT ? OFFSET ?");

        List<Object> paginatedValues = new ArrayList<>(values);
        paginatedValues.add(size);
        paginatedValues.add(offset);

        try {
            List<RecordDto> records = jdbcTemplate.query(
                    queryBuilder.toString(),
                    this::mapRowToRecord,
                    paginatedValues.toArray()
            );

            String countQuery = "SELECT COUNT(*) FROM " + validatedSchemaName + "." + validatedViewName +
                    " WHERE " + String.join(" AND ", whereClauses);

            Long totalRecords = jdbcTemplate.queryForObject(countQuery, Long.class, values.toArray());
            if (totalRecords == null) {
                totalRecords = 0L;
            }
            int totalPages = (int) Math.ceil((double) totalRecords / size);

            return ViewRecordPageDto.builder()
                    .items(records)
                    .totalItems(totalRecords)
                    .currentPage(page)
                    .pageSize(size)
                    .totalPages(totalPages)
                    .viewName(validatedViewName)
                    .schemaName(validatedSchemaName)
                    .build();

        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to fetch view records by values: " + e.getMessage(), e);
        }
    }

    @Override
    public List<RecordDto> updateRecordByValues(UpdateRecordByValuesDto updateDto) {
        databaseAuthorizationService.checkWritePermission(updateDto.getSchemaName(), updateDto.getTableName());

        validateTableExists(updateDto.getSchemaName(), updateDto.getTableName());

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = updateDto.getSchemaName().trim().toLowerCase();
        String validatedTableName = updateDto.getTableName().trim().toLowerCase();

        // Verify the record(s) exist for tables with identifying values
        List<RecordDto> existingRecords = getRecordsByValues(validatedSchemaName, validatedTableName,
                updateDto.getIdentifyingValues(), !updateDto.isAllowMultiple());

        if (existingRecords.isEmpty()) {
            throw new RecordNotFoundException("No records found matching the provided identifying values in table: " + validatedTableName);
        }

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
                " WHERE " + String.join(" AND ", whereClauses);

        // Add LIMIT clause for safety unless explicitly allowing multiple updates
        if (!updateDto.isAllowMultiple()) {
            query += " LIMIT 1";
        }

        try {
            int updatedRows = jdbcTemplate.update(query, values.toArray());

            if (updatedRows == 0) {
                throw new RecordNotFoundException("No records found matching the provided identifying values in table: " +
                        validatedTableName);
            }

            return getRecordsByValues(validatedSchemaName, validatedTableName,
                    updateDto.getNewData(), !updateDto.isAllowMultiple());

        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to update record by values: " + e.getMessage(), e);
        }
    }

    @Override
    public int deleteRecordByValues(DeleteRecordByValuesDto deleteDto) {
        databaseAuthorizationService.checkDeletePermission(deleteDto.getSchemaName(), deleteDto.getTableName());

        validateTableExists(deleteDto.getSchemaName(), deleteDto.getTableName());

        // schema name and table name are validated during the table existence check
        String validatedSchemaName = deleteDto.getSchemaName().trim().toLowerCase();
        String validatedTableName = deleteDto.getTableName().trim().toLowerCase();

        if (deleteDto.getIdentifyingValues() == null || deleteDto.getIdentifyingValues().isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "Identifying values cannot be null or empty");
        }

        List<String> whereClauses = new ArrayList<>();
        List<Object> values = new ArrayList<>();

        for (Map.Entry<String, Object> entry : deleteDto.getIdentifyingValues().entrySet()) {
            String columnName = SqlSecurityUtils.validateColumnName(entry.getKey());
            if (entry.getValue() == null) {
                whereClauses.add(columnName + " IS NULL");
            } else {
                whereClauses.add(columnName + " = ?");
                values.add(entry.getValue());
            }
        }

        String query = "DELETE FROM " + validatedSchemaName + "." + validatedTableName +
                " WHERE " + String.join(" AND ", whereClauses);

        // Add LIMIT clause for safety unless explicitly allowing multiple deletions
        if (!deleteDto.isAllowMultiple()) {
            query += " LIMIT 1";
        }

        try {
            return jdbcTemplate.update(query, values.toArray());

        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to delete record by values: " + e.getMessage(), e);
        }
    }

    @Override
    public List<RecordDto> createRecords(BatchNewRecordsDto batchNewRecords) {
        if (batchNewRecords == null || batchNewRecords.getRecords() == null || batchNewRecords.getRecords().isEmpty()) {
            throw new InvalidRecordDataException("No records provided for batch creation");
        }

        String validatedSchemaName = batchNewRecords.getSchemaName().trim().toLowerCase();
        String validatedTableName = batchNewRecords.getTableName().trim().toLowerCase();

        databaseAuthorizationService.checkCreatePermission(validatedSchemaName, validatedTableName);

        validateTableExists(validatedSchemaName, validatedTableName);

        List<BaseTableColumnMetadataDto> columns = metadataProviderService.getColumnsByTable(
                validatedSchemaName, validatedTableName, false, false);

        List<BaseTableColumnMetadataDto> primaryKeyColumns = getPrimaryKeyColumns(validatedSchemaName, validatedTableName);

        // Phase 1: Validate ALL records first before any database operations
        List<ValidatedRecordData> validatedRecords = new ArrayList<>();

        for (Map<String, Object> recordData : batchNewRecords.getRecords()) {
            validateRecordData(validatedSchemaName, validatedTableName, recordData, columns, false);

            // Prepare column names and values
            List<String> columnNames = new ArrayList<>();
            List<Object> values = new ArrayList<>();
            List<String> placeholders = new ArrayList<>();

            for (BaseTableColumnMetadataDto column : columns) {
                String columnName = column.getColumnName();
                if (recordData.containsKey(columnName)) {
                    columnNames.add(columnName);
                    values.add(recordData.get(columnName));
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

            // Store validated data for later execution
            validatedRecords.add(new ValidatedRecordData(query, values.toArray(), recordData, columnNames));
        }

        // Phase 2: Execute all insertions after validation passes
        List<RecordDto> createdRecords = new ArrayList<>();

        try {
            if (validatedRecords.isEmpty()) {
                return createdRecords;
            }
            String query = validatedRecords.get(0).query;
            KeyHolder keyHolder = new GeneratedKeyHolder();

            jdbcTemplate.batchUpdate(
                    connection -> connection.prepareStatement(query, Statement.RETURN_GENERATED_KEYS),
                    new BatchPreparedStatementSetter() {
                        @Override
                        public void setValues(PreparedStatement ps, int i) throws SQLException {
                            Object[] values = validatedRecords.get(i).values;
                            for (int j = 0; j < values.length; j++) {
                                ps.setObject(j + 1, values[j]);
                            }
                        }

                        @Override
                        public int getBatchSize() {
                            return validatedRecords.size();
                        }
                    },
                    keyHolder
            );

            List<Map<String, Object>> generatedKeys = keyHolder.getKeyList();
            for (int i = 0; i < validatedRecords.size(); i++) {
                ValidatedRecordData validatedRecord = validatedRecords.get(i);
                RecordDto createdRecord;
                if (primaryKeyColumns.isEmpty()) {
                    Map<String, Object> identifyingValues = new HashMap<>();
                    for (String columnName : validatedRecord.columnNames) {
                        identifyingValues.put(columnName, validatedRecord.originalData.get(columnName));
                    }
                    createdRecord = getRecordByValues(validatedSchemaName, validatedTableName, identifyingValues);
                } else if (primaryKeyColumns.size() == 1 && primaryKeyColumns.get(0).getAutoIncrement()) {
                    Map<String, Object> generatedKey = generatedKeys.get(i);
                    Number generatedId = (Number) generatedKey.values().iterator().next();
                    if (generatedId == null) {
                        throw new RuntimeException("Failed to retrieve generated key");
                    }
                    Map<String, Object> pkValues = new HashMap<>();
                    pkValues.put(primaryKeyColumns.get(0).getColumnName(), generatedId.longValue());
                    createdRecord = getRecord(validatedSchemaName, validatedTableName, pkValues);
                } else {
                    // Build primary key values from the inserted data
                    Map<String, Object> pkValues = new HashMap<>();
                    for (BaseTableColumnMetadataDto pkColumn : primaryKeyColumns) {
                        String columnName = pkColumn.getColumnName();
                        if (validatedRecord.originalData.containsKey(columnName)) {
                            pkValues.put(columnName, validatedRecord.originalData.get(columnName));
                        }
                    }
                    createdRecord = getRecord(validatedSchemaName, validatedTableName, pkValues);
                }
                createdRecords.add(createdRecord);
            }
        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to execute record creation in batch: " + e.getMessage(), e);
        }

        return createdRecords;
    }

    @Override
    public List<RecordDto> updateRecords(BatchUpdateRecordsDto batchUpdateRecords) {
        if (batchUpdateRecords == null || batchUpdateRecords.getUpdates() == null || batchUpdateRecords.getUpdates().isEmpty()) {
            throw new InvalidRecordDataException("No records provided for batch update");
        }

        String validatedSchemaName = batchUpdateRecords.getSchemaName().trim().toLowerCase();
        String validatedTableName = batchUpdateRecords.getTableName().trim().toLowerCase();

        databaseAuthorizationService.checkWritePermission(validatedSchemaName, validatedTableName);

        validateTableExists(validatedSchemaName, validatedTableName);

        List<BaseTableColumnMetadataDto> primaryKeyColumns = getPrimaryKeyColumns(validatedSchemaName, validatedTableName);

        if (primaryKeyColumns.isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "Table has no primary key columns, use update by values");
        }

        // Phase 1: Validate ALL updates first before any database operations
        List<ValidatedUpdateData> validatedUpdates = new ArrayList<>();

        for (BatchUpdateRecordsDto.SingleUpdateRecordDto updateRecord : batchUpdateRecords.getUpdates()) {
            // Verify the record exists
            getRecord(validatedSchemaName, validatedTableName, updateRecord.getPrimaryKeyValues());

            validateRecordData(validatedSchemaName, validatedTableName, updateRecord.getData(), null, true);

            List<String> setClauses = new ArrayList<>();
            List<Object> values = new ArrayList<>();

            for (Map.Entry<String, Object> entry : updateRecord.getData().entrySet()) {
                String columnName = SqlSecurityUtils.validateColumnName(entry.getKey());
                setClauses.add(columnName + " = ?");
                values.add(entry.getValue());
            }

            if (setClauses.isEmpty()) {
                throw new InvalidRecordDataException(validatedTableName, "No data provided for update");
            }

            // Build WHERE clause for primary key
            List<String> whereClauses = new ArrayList<>();
            for (Map.Entry<String, Object> pkEntry : updateRecord.getPrimaryKeyValues().entrySet()) {
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

            // Store validated data for later execution
            validatedUpdates.add(new ValidatedUpdateData(query, values.toArray(), updateRecord.getPrimaryKeyValues()));
        }

        // Phase 2: Execute all updates after validation passes
        List<RecordDto> updatedRecords = new ArrayList<>();

        try {
            String query = validatedUpdates.get(0).query;
            List<Object[]> batchArgs = validatedUpdates.stream()
                    .map(validatedUpdate -> validatedUpdate.values)
                    .toList();

            jdbcTemplate.batchUpdate(query, batchArgs);

            // Phase 3: Retrieve updated records
            for (BatchUpdateRecordsDto.SingleUpdateRecordDto updateRecord : batchUpdateRecords.getUpdates()) {
                RecordDto updated = getRecord(validatedSchemaName, validatedTableName, updateRecord.getData());
                updatedRecords.add(updated);
            }
        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to execute record updates in batch: " + e.getMessage(), e);
        }

        return updatedRecords;
    }

    @Override
    public int deleteRecords(BatchDeleteRecordsDto batchDeleteRecords) {
        if (batchDeleteRecords == null || batchDeleteRecords.getPrimaryKeyValuesList() == null ||
                batchDeleteRecords.getPrimaryKeyValuesList().isEmpty()) {
            return 0;
        }

        String validatedSchemaName = batchDeleteRecords.getSchemaName().trim().toLowerCase();
        String validatedTableName = batchDeleteRecords.getTableName().trim().toLowerCase();

        databaseAuthorizationService.checkDeletePermission(validatedSchemaName, validatedTableName);

        validateTableExists(validatedSchemaName, validatedTableName);

        List<BaseTableColumnMetadataDto> primaryKeyColumns = getPrimaryKeyColumns(validatedSchemaName, validatedTableName);

        if (primaryKeyColumns.isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "Table has no primary key columns, use delete by values");
        }

        // Phase 1: Validate ALL deletions first before any database operations
        List<ValidatedDeleteData> validatedDeletions = new ArrayList<>();

        // Get required primary key columns for validation
        Set<String> requiredPkColumns = primaryKeyColumns.stream()
                .map(BaseTableColumnMetadataDto::getColumnName)
                .map(String::toLowerCase)
                .collect(Collectors.toSet());

        for (Map<String, Object> primaryKeyValues : batchDeleteRecords.getPrimaryKeyValuesList()) {
            if (primaryKeyValues == null || primaryKeyValues.isEmpty()) {
                throw new InvalidRecordDataException(validatedTableName, "Primary key values cannot be null or empty");
            }

            // Validate that all required primary key columns are provided
            Set<String> providedPkColumns = primaryKeyValues.keySet().stream()
                    .map(String::trim)
                    .map(String::toLowerCase)
                    .collect(Collectors.toSet());

            if (!providedPkColumns.containsAll(requiredPkColumns)) {
                Set<String> missingColumns = new HashSet<>(requiredPkColumns);
                missingColumns.removeAll(providedPkColumns);
                throw new InvalidRecordDataException(validatedTableName,
                        "Missing required primary key columns: " + String.join(", ", missingColumns));
            }

            if (!requiredPkColumns.containsAll(providedPkColumns)) {
                Set<String> invalidColumns = new HashSet<>(providedPkColumns);
                invalidColumns.removeAll(requiredPkColumns);
                throw new InvalidRecordDataException(validatedTableName,
                        "Invalid primary key columns provided: " + String.join(", ", invalidColumns));
            }

            List<String> whereClauses = new ArrayList<>();
            List<Object> values = new ArrayList<>();

            for (Map.Entry<String, Object> pkEntry : primaryKeyValues.entrySet()) {
                String columnName = SqlSecurityUtils.validateColumnName(pkEntry.getKey());
                if (pkEntry.getValue() == null) {
                    whereClauses.add(columnName + " IS NULL");
                } else {
                    whereClauses.add(columnName + " = ?");
                    values.add(pkEntry.getValue());
                }
            }

            String query = "DELETE FROM " + validatedSchemaName + "." + validatedTableName +
                    " WHERE " + String.join(" AND ", whereClauses);

            // Store validated data for later execution
            validatedDeletions.add(new ValidatedDeleteData(query, values.toArray(), primaryKeyValues));
        }

        // Phase 2: Execute all deletions after validation passes
        int deletedCount;

        try {
            String query = validatedDeletions.get(0).query;
            List<Object[]> batchArgs = validatedDeletions.stream()
                    .map(validatedDelete -> validatedDelete.values)
                    .toList();

            int[] deletedRows = jdbcTemplate.batchUpdate(query, batchArgs);
            deletedCount = Arrays.stream(deletedRows).sum();
        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to execute record deletions in batch: " + e.getMessage(), e);
        }

        return deletedCount;
    }

    @Override
    public List<RecordDto> updateRecordsByValues(BatchUpdateRecordsByValuesDto batchUpdateByValues) {
        if (batchUpdateByValues == null || batchUpdateByValues.getUpdates() == null || batchUpdateByValues.getUpdates().isEmpty()) {
            throw new InvalidRecordDataException("No records provided for batch update by values");
        }

        String validatedSchemaName = batchUpdateByValues.getSchemaName().trim().toLowerCase();
        String validatedTableName = batchUpdateByValues.getTableName().trim().toLowerCase();

        databaseAuthorizationService.checkWritePermission(validatedSchemaName, validatedTableName);

        validateTableExists(validatedSchemaName, validatedTableName);

        // Phase 1: Validate ALL updates first before any database operations
        List<ValidatedUpdateByValuesData> validatedUpdates = new ArrayList<>();

        for (BatchUpdateRecordsByValuesDto.SingleUpdateRecordByValuesDto updateRecord : batchUpdateByValues.getUpdates()) {
            // Verify the record(s) exist for tables with identifying values
            List<RecordDto> existingRecords = getRecordsByValues(validatedSchemaName, validatedTableName,
                    updateRecord.getIdentifyingValues(), !updateRecord.isAllowMultiple());

            if (existingRecords.isEmpty()) {
                throw new RecordNotFoundException("No records found matching the provided identifying values in table: " + validatedTableName);
            }

            validateRecordData(validatedSchemaName, validatedTableName, updateRecord.getNewData(), null, true);

            List<String> setClauses = new ArrayList<>();
            List<Object> values = new ArrayList<>();

            for (Map.Entry<String, Object> entry : updateRecord.getNewData().entrySet()) {
                String columnName = SqlSecurityUtils.validateColumnName(entry.getKey());
                setClauses.add(columnName + " = ?");
                values.add(entry.getValue());
            }

            if (setClauses.isEmpty()) {
                throw new InvalidRecordDataException(validatedTableName, "No data provided for update");
            }

            // Build WHERE clause using identifying values
            List<String> whereClauses = new ArrayList<>();
            for (Map.Entry<String, Object> entry : updateRecord.getIdentifyingValues().entrySet()) {
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
                    " WHERE " + String.join(" AND ", whereClauses);

            // Add LIMIT clause for safety unless explicitly allowing multiple updates
            if (!updateRecord.isAllowMultiple()) {
                query += " LIMIT 1";
            }

            // Store validated data for later execution
            validatedUpdates.add(new ValidatedUpdateByValuesData(query, values.toArray(), updateRecord.getIdentifyingValues(), updateRecord.isAllowMultiple()));
        }

        // Phase 2: Execute all updates after validation passes
        List<RecordDto> updatedRecords = new ArrayList<>();

        try {
            // IMPORTANT: Queries may differ between updates because identifying values that are null
            // generate 'IS NULL' (no placeholder) while non-null values generate '= ?'. Reusing the
            // first query for all updates (previous implementation) causes a mismatch between the
            // number of placeholders and the number of bound parameters leading to
            // 'Parameter index out of range' errors. We group updates by their exact SQL so each
            // batch only contains argument lists matching the placeholder pattern of that SQL.

            Map<String, List<ValidatedUpdateByValuesData>> grouped = validatedUpdates.stream()
                    .collect(Collectors.groupingBy(ValidatedUpdateByValuesData::query));

            for (Map.Entry<String, List<ValidatedUpdateByValuesData>> entry : grouped.entrySet()) {
                String sql = entry.getKey();
                List<Object[]> args = entry.getValue().stream()
                        .map(v -> v.values)
                        .toList();
                jdbcTemplate.batchUpdate(sql, args);
            }

            // Phase 3: Retrieve updated records
            for (BatchUpdateRecordsByValuesDto.SingleUpdateRecordByValuesDto updateRecord : batchUpdateByValues.getUpdates()) {
                List<RecordDto> updated = getRecordsByValues(validatedSchemaName, validatedTableName,
                        updateRecord.getNewData(), !updateRecord.isAllowMultiple());
                updatedRecords.addAll(updated);
            }
        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to execute record updates by values in batch: " + e.getMessage(), e);
        }

        return updatedRecords;
    }

    @Override
    public int deleteRecordsByValues(BatchDeleteRecordsByValuesDto batchDeleteByValues) {
        if (batchDeleteByValues == null || batchDeleteByValues.getDeletions() == null || batchDeleteByValues.getDeletions().isEmpty()) {
            return 0;
        }

        String validatedSchemaName = batchDeleteByValues.getSchemaName().trim().toLowerCase();
        String validatedTableName = batchDeleteByValues.getTableName().trim().toLowerCase();

        databaseAuthorizationService.checkDeletePermission(validatedSchemaName, validatedTableName);

        validateTableExists(validatedSchemaName, validatedTableName);

        // Phase 1: Validate ALL deletions first before any database operations
        List<ValidatedDeleteByValuesData> validatedDeletions = new ArrayList<>();

        for (BatchDeleteRecordsByValuesDto.SingleDeleteRecordByValuesDto deleteRecord : batchDeleteByValues.getDeletions()) {
            if (deleteRecord.getIdentifyingValues() == null || deleteRecord.getIdentifyingValues().isEmpty()) {
                throw new InvalidRecordDataException(validatedTableName, "Identifying values cannot be null or empty");
            }

            List<String> whereClauses = new ArrayList<>();
            List<Object> values = new ArrayList<>();

            for (Map.Entry<String, Object> entry : deleteRecord.getIdentifyingValues().entrySet()) {
                String columnName = SqlSecurityUtils.validateColumnName(entry.getKey());
                if (entry.getValue() == null) {
                    whereClauses.add(columnName + " IS NULL");
                } else {
                    whereClauses.add(columnName + " = ?");
                    values.add(entry.getValue());
                }
            }

            String query = "DELETE FROM " + validatedSchemaName + "." + validatedTableName +
                    " WHERE " + String.join(" AND ", whereClauses);

            // Add LIMIT clause for safety unless explicitly allowing multiple deletions
            if (!deleteRecord.isAllowMultiple()) {
                query += " LIMIT 1";
            }

            // Store validated data for later execution
            validatedDeletions.add(new ValidatedDeleteByValuesData(query, values.toArray(), deleteRecord.getIdentifyingValues()));
        }

        // Phase 2: Execute all deletions after validation passes
        int deletedCount;

        try {
            // IMPORTANT: Similar to updateRecordsByValues, different deletions may have
            // different SQL due to null identifying values producing 'IS NULL' (no placeholder)
            // versus '= ?'. We must execute batches grouped by their exact SQL to align
            // placeholder counts with provided parameters.
            Map<String, List<ValidatedDeleteByValuesData>> grouped = validatedDeletions.stream()
                    .collect(Collectors.groupingBy(ValidatedDeleteByValuesData::query));

            deletedCount = 0;
            for (Map.Entry<String, List<ValidatedDeleteByValuesData>> entry : grouped.entrySet()) {
                String sql = entry.getKey();
                List<Object[]> args = entry.getValue().stream()
                        .map(v -> v.values)
                        .toList();
                int[] batchResults = jdbcTemplate.batchUpdate(sql, args);
                deletedCount += Arrays.stream(batchResults).sum();
            }
        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to execute record deletions by values in batch: " + e.getMessage(), e);
        }

        return deletedCount;
    }

    @Override
    public long getRecordCount(String schemaName, String tableName, boolean checkTableExists) {
        return getRecordCount(schemaName, tableName, checkTableExists, false);
    }

    @Override
    public long getRecordCount(String schemaName, String tableName, boolean checkTableExists, boolean checkAuthorization) {
        String validatedSchemaName = schemaName.trim().toLowerCase();
        String validatedTableName = tableName.trim().toLowerCase();

        if (checkAuthorization)
            databaseAuthorizationService.checkReadPermission(validatedSchemaName, validatedTableName);

        if (checkTableExists) validateTableExists(validatedSchemaName, validatedTableName);

        String query = "SELECT COUNT(*) FROM " + validatedSchemaName + "." + validatedTableName;

        try {
            Long count = jdbcTemplate.queryForObject(query, Long.class);
            return count != null ? count : 0L;
        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to get record count: " + e.getMessage(), e);
        }
    }

    @Override
    public long getViewRecordCount(String schemaName, String viewName, boolean checkViewExists, boolean checkAuthorization) {
        String validatedSchemaName = schemaName.trim().toLowerCase();
        String validatedViewName = viewName.trim().toLowerCase();

        if (checkAuthorization)
            databaseAuthorizationService.checkReadPermission(validatedSchemaName, validatedViewName);

        if (checkViewExists) validateViewExists(validatedSchemaName, validatedViewName);

        try {
            String sql = "SELECT COUNT(*) FROM " + validatedSchemaName + "." + validatedViewName;
            Long count = jdbcTemplate.queryForObject(sql, Long.class);
            return count != null ? count : 0L;
        } catch (DataAccessException e) {
            throw new RuntimeException("Failed to get view record count: " + e.getMessage(), e);
        }
    }

    @Override
    public void validateRecordData(String schemaName, String tableName, Map<String, Object> data,
                                   List<BaseTableColumnMetadataDto> columns, boolean isUpdate) {
        if (data == null || data.isEmpty()) {
            throw new InvalidRecordDataException(tableName, "Record data cannot be null or empty");
        }

        if (columns == null) {
            // columns are either provided from outside (already fetched),
            // or fetched here if 'columns=null'
            columns = metadataProviderService.getColumnsByTable(schemaName, tableName, false, false);
        }
        Map<String, BaseTableColumnMetadataDto> columnMap = columns.stream()
                .collect(Collectors.toMap(BaseTableColumnMetadataDto::getColumnName, col -> col));

        // Validate each provided column
        for (Map.Entry<String, Object> entry : data.entrySet()) {
            String columnName = entry.getKey();
            Object value = entry.getValue();

            BaseTableColumnMetadataDto column = columnMap.get(columnName);
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

            if (value != null) {
                validateValueDatatype(tableName, columnName, value, column);
            }

            if (column.getIsUnique() && !isUpdate) {
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
        }
    }

    private void validateValueDatatype(String tableName, String columnName, Object value, BaseTableColumnMetadataDto column) {
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
                } else if (!(value instanceof Number)) {
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
        }
    }

    private void validateValueUniqueness(String schemaName, String tableName, String columnName, Object value) {
        Integer count;
        try {
            String checkSql = String.format(
                    "SELECT 1 FROM %s.%s WHERE %s = ? LIMIT 1",
                    schemaName, tableName, columnName);

            count = jdbcTemplate.queryForObject(checkSql, Integer.class, value);
        } catch (Exception e) {
            count = 0;
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
                    "SELECT 1 FROM %s.%s WHERE %s = ? LIMIT 1",
                    schemaName, tableName, columnName);

            count = jdbcTemplate.queryForObject(checkSql, Integer.class, value);
        } catch (Exception e) {
            count = 0;
        }

        if (count == null || count == 0) {
            throw new InvalidRecordDataException(
                    String.format("Column value '%s' does not exist in the referenced table '%s.%s' column '%s'",
                            value, schemaName, tableName, columnName));
        }
    }

    private List<BaseTableColumnMetadataDto> getPrimaryKeyColumns(String schemaName, String tableName) {
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

    private void validateViewExists(String schemaName, String viewName) {
        if (!metadataProviderService.viewExists(schemaName, viewName)) {
            throw new ViewNotFoundException(schemaName.toLowerCase(), viewName.toLowerCase());
        }
    }

    private void validateViewColumnExists(String schemaName, String viewName, String columnName) {
        if (!metadataProviderService.viewColumnExists(schemaName, viewName, columnName)) {
            throw new ColumnNotFoundException(
                    schemaName.toLowerCase(),
                    viewName.toLowerCase(),
                    columnName.toLowerCase());
        }
    }

    private String validateSortDirection(String sortDirection) {
        if (sortDirection == null || sortDirection.isBlank() || !sortDirection.equalsIgnoreCase("DESC")) {
            return "ASC";
        }
        return "DESC";
    }

    @Override
    public AdvancedSearchResponseDto advancedSearch(AdvancedSearchRequestDto searchRequest) {
        databaseAuthorizationService.checkReadPermission(searchRequest.getSchemaName(), searchRequest.getObjectName());

        validateTableExists(searchRequest.getSchemaName(), searchRequest.getObjectName());

        String validatedSchemaName = searchRequest.getSchemaName().trim().toLowerCase();
        String validatedTableName = searchRequest.getObjectName().trim().toLowerCase();

        List<BaseColumnMetadataDto> tableColumns = metadataProviderService.getColumnsByTable(
                        validatedSchemaName, validatedTableName, false, false)
                .stream().map(col -> (BaseColumnMetadataDto) col).toList();
        Map<String, BaseColumnMetadataDto> columnMap = tableColumns.stream()
                .collect(Collectors.toMap(BaseColumnMetadataDto::getColumnName, col -> col));

        // Build the query components
        SearchQueryBuilder queryBuilder = new SearchQueryBuilder(validatedSchemaName, validatedTableName);

        // Apply filters
        if (searchRequest.getFilters() != null && !searchRequest.getFilters().isEmpty()) {
            // Validate filters
            List<String> errors = SearchQueryBuilder.getValidationErrors(
                    searchRequest.getFilters(),
                    columnMap);

            if (!errors.isEmpty()) {
                throw new InvalidRecordDataException(
                        validatedTableName,
                        "Invalid filter criteria:\n\n" + String.join("\n", errors));
            }

            for (FilterCriteriaDto filter : searchRequest.getFilters()) {
                queryBuilder.addFilter(filter, columnMap.get(filter.getColumnName().trim()), false);
            }
        }

        // Apply global search
        if (searchRequest.getGlobalSearch() != null && !searchRequest.getGlobalSearch().isBlank()) {
            queryBuilder.addGlobalSearch(searchRequest.getGlobalSearch(), getTextColumns(tableColumns));
        }

        // Apply sorting
        if (searchRequest.getSorts() != null && !searchRequest.getSorts().isEmpty()) {
            for (SortCriteriaDto sort : searchRequest.getSorts()) {
                validateColumnExists(validatedSchemaName, validatedTableName, sort.getColumnName());
                queryBuilder.addSort(sort);
            }
        }

        // Apply distinct
        queryBuilder.setDistinct(searchRequest.isDistinct());

        // Build and execute count query (for total filtered records)
        String countQuery = queryBuilder.buildCountQuery();
        Long filteredRecords = jdbcTemplate.queryForObject(countQuery, Long.class,
                queryBuilder.getParameters().toArray());

        if (filteredRecords == null) {
            filteredRecords = 0L;
        }

        // Apply pagination
        queryBuilder.addPagination(searchRequest.getPage(), searchRequest.getSize());

        // Build and execute main query
        String mainQuery = queryBuilder.buildSelectQuery();
        List<RecordDto> records = jdbcTemplate.query(mainQuery, this::mapRowToRecord,
                queryBuilder.getParameters().toArray());

        // Get total records in table (unfiltered)
        long totalRecords = getRecordCount(validatedSchemaName, validatedTableName, false);

        // Calculate pagination info
        int totalPages = (int) Math.ceil((double) filteredRecords / searchRequest.getSize());

        return AdvancedSearchResponseDto.builder()
                .records(records)
                .totalRecords(totalRecords)
                .filteredRecords(filteredRecords)
                .currentPage(searchRequest.getPage())
                .pageSize(searchRequest.getSize())
                .totalPages(totalPages)
                .objectName(validatedTableName)
                .schemaName(validatedSchemaName)
                .hasFilters(searchRequest.getFilters() != null && !searchRequest.getFilters().isEmpty())
                .hasGlobalSearch(searchRequest.getGlobalSearch() != null && !searchRequest.getGlobalSearch().isBlank())
                .hasSort(searchRequest.getSorts() != null && !searchRequest.getSorts().isEmpty())
                .isDistinct(searchRequest.isDistinct())
                .appliedFilters(searchRequest.getFilters())
                .appliedSorts(searchRequest.getSorts())
                .appliedGlobalSearch(searchRequest.getGlobalSearch())
                .build();
    }

    @Override
    public AdvancedSearchResponseDto advancedSearchView(AdvancedSearchRequestDto searchRequest) {
        databaseAuthorizationService.checkReadPermission(searchRequest.getSchemaName(), searchRequest.getObjectName());
        validateViewExists(searchRequest.getSchemaName(), searchRequest.getObjectName());

        String validatedSchemaName = searchRequest.getSchemaName().trim().toLowerCase();
        String validatedViewName = searchRequest.getObjectName().trim().toLowerCase();

        List<BaseColumnMetadataDto> viewColumns = metadataProviderService.getColumnsByView(
                        validatedSchemaName, validatedViewName, false, false)
                .stream().map(col -> (BaseColumnMetadataDto) col).toList();
        Map<String, BaseColumnMetadataDto> columnMap = viewColumns.stream()
                .collect(Collectors.toMap(BaseColumnMetadataDto::getColumnName, col -> col));

        SearchQueryBuilder queryBuilder = new SearchQueryBuilder(validatedSchemaName, validatedViewName);

        // Apply filters
        if (searchRequest.getFilters() != null && !searchRequest.getFilters().isEmpty()) {
            List<String> errors = SearchQueryBuilder.getValidationErrors(
                    searchRequest.getFilters(),
                    columnMap);

            if (!errors.isEmpty()) {
                throw new InvalidRecordDataException(
                        validatedViewName,
                        "Invalid filter criteria:\n\n" + String.join("\n", errors));
            }

            for (FilterCriteriaDto filter : searchRequest.getFilters()) {
                queryBuilder.addFilter(filter, columnMap.get(filter.getColumnName().trim()), false);
            }
        }

        // Apply global search
        if (searchRequest.getGlobalSearch() != null && !searchRequest.getGlobalSearch().isBlank()) {
            queryBuilder.addGlobalSearch(searchRequest.getGlobalSearch(), getTextColumns(viewColumns));
        }

        // Apply sorting
        if (searchRequest.getSorts() != null && !searchRequest.getSorts().isEmpty()) {
            for (SortCriteriaDto sort : searchRequest.getSorts()) {
                validateViewColumnExists(validatedSchemaName, validatedViewName, sort.getColumnName());
                queryBuilder.addSort(sort);
            }
        }

        queryBuilder.setDistinct(searchRequest.isDistinct());

        String countQuery = queryBuilder.buildCountQuery();
        Long filteredRecords = jdbcTemplate.queryForObject(countQuery, Long.class,
                queryBuilder.getParameters().toArray());

        if (filteredRecords == null) {
            filteredRecords = 0L;
        }

        queryBuilder.addPagination(searchRequest.getPage(), searchRequest.getSize());

        String mainQuery = queryBuilder.buildSelectQuery();
        List<RecordDto> records = jdbcTemplate.query(mainQuery, this::mapRowToRecord,
                queryBuilder.getParameters().toArray());

        long totalRecords = getViewRecordCount(validatedSchemaName, validatedViewName, false, false);
        int totalPages = (int) Math.ceil((double) filteredRecords / searchRequest.getSize());

        return AdvancedSearchResponseDto.builder()
                .records(records)
                .totalRecords(totalRecords)
                .filteredRecords(filteredRecords)
                .currentPage(searchRequest.getPage())
                .pageSize(searchRequest.getSize())
                .totalPages(totalPages)
                .objectName(validatedViewName)
                .schemaName(validatedSchemaName)
                .hasFilters(searchRequest.getFilters() != null && !searchRequest.getFilters().isEmpty())
                .hasGlobalSearch(searchRequest.getGlobalSearch() != null && !searchRequest.getGlobalSearch().isBlank())
                .hasSort(searchRequest.getSorts() != null && !searchRequest.getSorts().isEmpty())
                .isDistinct(searchRequest.isDistinct())
                .appliedFilters(searchRequest.getFilters())
                .appliedSorts(searchRequest.getSorts())
                .appliedGlobalSearch(searchRequest.getGlobalSearch())
                .build();
    }

    private List<String> getTextColumns(List<BaseColumnMetadataDto> columns) {
        return columns.stream()
                .filter(col -> isTextColumn(col.getDataType()))
                .map(BaseColumnMetadataDto::getColumnName)
                .collect(Collectors.toList());
    }

    private boolean isTextColumn(String dataType) {
        String type = dataType.toLowerCase();
        return type.contains("char") || type.contains("text") || type.contains("varchar")
                || type.contains("longtext") || type.contains("mediumtext") || type.contains("tinytext");
    }

    private record ValidatedRecordData(String query, Object[] values, Map<String, Object> originalData,
                                       List<String> columnNames) {
    }

    private record ValidatedUpdateData(String query, Object[] values, Map<String, Object> primaryKeyValues) {
    }

    private record ValidatedDeleteData(String query, Object[] values, Map<String, Object> primaryKeyValues) {
    }

    private record ValidatedUpdateByValuesData(String query, Object[] values, Map<String, Object> identifyingValues,
                                               boolean allowMultiple) {
    }

    private record ValidatedDeleteByValuesData(String query, Object[] values, Map<String, Object> identifyingValues) {
    }
}
