package ma.ilias.dbmanagementbe.metadata.service.column;

import ma.ilias.dbmanagementbe.metadata.dto.column.ColumnMetadataDto;

public interface ColumnService {
    Boolean columnExists(String schemaName, String tableName, String columnName);

    ColumnMetadataDto getColumn(String schemaName, String tableName, String columnName);
}
