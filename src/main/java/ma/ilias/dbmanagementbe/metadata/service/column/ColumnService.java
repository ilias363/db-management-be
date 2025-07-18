package ma.ilias.dbmanagementbe.metadata.service.column;

import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.*;

public interface ColumnService {
    Boolean columnExists(String schemaName, String tableName, String columnName);

    BaseColumnMetadataDto getColumn(String schemaName, String tableName, String columnName);

    BaseColumnMetadataDto createColumn(BaseNewColumnDto newColumnDto);

    Boolean deleteColumn(String schemaName, String tableName, String columnName, boolean force);

    Boolean isColumnPrimaryKey(String schemaName, String tableName, String columnName);

    BaseColumnMetadataDto renameColumn(RenameColumnDto renameColumnDto);

    BaseColumnMetadataDto updateColumnDataType(UpdateColumnDataTypeDto updateColumnDataTypeDto);

    BaseColumnMetadataDto updateColumnAutoIncrement(UpdateColumnAutoIncrementDto updateColumnAutoIncrementDto);

    BaseColumnMetadataDto updateColumnNullable(UpdateColumnNullableDto updateColumnNullableDto, boolean populate);

    BaseColumnMetadataDto updateColumnUnique(UpdateColumnUniqueDto updateColumnUniqueDto);

    BaseColumnMetadataDto updateColumnDefault(UpdateColumnDefaultDto updateColumnDefaultDto);
}