package ma.ilias.dbmanagementbe.metadata.service.column;

import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseTableColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.*;

import java.util.List;

public interface ColumnService {
    Boolean columnExists(String schemaName, String tableName, String columnName);

    BaseTableColumnMetadataDto getColumn(String schemaName, String tableName, String columnName,
                                         boolean includeTable, boolean checkColumnExists, boolean checkAuthorization);

    BaseTableColumnMetadataDto getColumn(String schemaName, String tableName, String columnName,
                                         boolean includeTable, boolean checkColumnExists);

    List<BaseTableColumnMetadataDto> getColumnsByTable(String schemaName, String tableName,
                                                       boolean includeTable, boolean checkTableExists);

    BaseTableColumnMetadataDto createColumn(BaseNewColumnDto newColumnDto);

    Boolean deleteColumn(String schemaName, String tableName, String columnName, boolean force);

    Boolean isColumnPrimaryKey(String schemaName, String tableName, String columnName);

    BaseTableColumnMetadataDto renameColumn(RenameColumnDto renameColumnDto);

    BaseTableColumnMetadataDto updateColumnDataType(UpdateColumnDataTypeDto updateColumnDataTypeDto);

    BaseTableColumnMetadataDto updateColumnAutoIncrement(UpdateColumnAutoIncrementDto updateColumnAutoIncrementDto);

    BaseTableColumnMetadataDto updateColumnNullable(UpdateColumnNullableDto updateColumnNullableDto, boolean populate);

    BaseTableColumnMetadataDto updateColumnUnique(UpdateColumnUniqueDto updateColumnUniqueDto);

    BaseTableColumnMetadataDto updateColumnDefault(UpdateColumnDefaultDto updateColumnDefaultDto);

    List<BaseTableColumnMetadataDto> updateColumnPrimaryKey(UpdateColumnPrimaryKeyDto updateColumnPrimaryKeyDto, boolean force);

    BaseTableColumnMetadataDto updateColumnForeignKey(UpdateColumnForeignKeyDto updateColumnForeignKeyDto);
}
