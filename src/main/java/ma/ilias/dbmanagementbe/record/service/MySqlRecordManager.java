package ma.ilias.dbmanagementbe.record.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.ColumnNotFoundException;
import ma.ilias.dbmanagementbe.exception.InvalidRecordDataException;
import ma.ilias.dbmanagementbe.exception.RecordNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.record.dto.RecordDto;
import ma.ilias.dbmanagementbe.record.dto.RecordPageDto;
import ma.ilias.dbmanagementbe.util.SqlSecurityUtils;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(schemaName).toLowerCase();
        String validatedTableName = SqlSecurityUtils.validateTableName(tableName).toLowerCase();

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

        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(schemaName);
        String validatedTableName = SqlSecurityUtils.validateTableName(tableName);

        List<BaseColumnMetadataDto> primaryKeyColumns = getPrimaryKeyColumns(validatedSchemaName, validatedTableName);

        if (primaryKeyColumns.isEmpty()) {
            throw new InvalidRecordDataException(validatedTableName, "Table has no primary key defined");
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

    private List<BaseColumnMetadataDto> getPrimaryKeyColumns(String schemaName, String tableName) {
        return metadataProviderService.getColumnsByTable(schemaName, tableName, false, false)
                .stream()
                .filter(column -> metadataProviderService.isColumnPrimaryKey(schemaName, tableName, column.getColumnName()))
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
