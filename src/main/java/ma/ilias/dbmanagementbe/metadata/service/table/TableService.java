package ma.ilias.dbmanagementbe.metadata.service.table;

import ma.ilias.dbmanagementbe.metadata.dto.table.NewTableDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;

import java.util.List;

public interface TableService {
    Boolean tableExists(String schemaName, String tableName);

    TableMetadataDto getTable(String schemaName, String tableName);

    List<TableMetadataDto> getTablesBySchema(String schemaName);

    TableMetadataDto createTable(NewTableDto newTable);
}
