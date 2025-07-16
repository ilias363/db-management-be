package ma.ilias.dbmanagementbe.metadata.service.column;

import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;

public interface ColumnService {
    Boolean columnExists(String schemaName, String tableName, String columnName);

    BaseColumnMetadataDto getColumn(String schemaName, String tableName, String columnName);

    Boolean deleteColumn(String schemaName, String tableName, String columnName, boolean force);
}
