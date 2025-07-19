package ma.ilias.dbmanagementbe.metadata.service.index;

import ma.ilias.dbmanagementbe.metadata.dto.index.IndexMetadataDto;

public interface IndexService {
    Boolean indexExists(String schemaName, String tableName, String indexName);

    IndexMetadataDto getIndex(String schemaName, String tableName, String indexName, boolean includeTable,
            boolean checkIndexExists);
}
