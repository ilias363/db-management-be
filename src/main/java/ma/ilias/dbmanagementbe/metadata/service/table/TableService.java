package ma.ilias.dbmanagementbe.metadata.service.table;

import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;

public interface TableService {
    Boolean tableExists(String schemaName, String tableName);

    TableMetadataDto getTable(String schemaName, String tableName);
}
