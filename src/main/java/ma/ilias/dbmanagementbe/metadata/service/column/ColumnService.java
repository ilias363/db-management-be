package ma.ilias.dbmanagementbe.metadata.service.column;

import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;

public interface ColumnService {
    Boolean columnExists(String schemaName, String tableName, String columnName);

    BaseColumnMetadataDto getColumn(String schemaName, String tableName, String columnName);

    BaseColumnMetadataDto createColumn(BaseNewColumnDto newColumnDto);

    Boolean deleteColumn(String schemaName, String tableName, String columnName, boolean force);

    Boolean isColumnPrimaryKey(String schemaName, String tableName, String columnName);
}
