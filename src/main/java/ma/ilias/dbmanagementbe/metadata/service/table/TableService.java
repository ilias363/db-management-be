package ma.ilias.dbmanagementbe.metadata.service.table;

import ma.ilias.dbmanagementbe.metadata.dto.table.NewTableDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.UpdateTableDto;

import java.util.List;

public interface TableService {
    Boolean tableExists(String schemaName, String tableName);

    TableMetadataDto getTable(String schemaName, String tableName, boolean includeColumns, boolean includeIndexes);

    List<TableMetadataDto> getTablesBySchema(String schemaName);

    TableMetadataDto createTable(NewTableDto newTable);

    TableMetadataDto renameTable(UpdateTableDto updateTableDto);

    Boolean deleteTable(String schemaName, String tableName, boolean force);
}
