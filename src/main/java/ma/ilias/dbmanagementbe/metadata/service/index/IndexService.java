package ma.ilias.dbmanagementbe.metadata.service.index;

import ma.ilias.dbmanagementbe.metadata.dto.index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.NewIndexDto;

import java.util.List;

public interface IndexService {
    Boolean indexExists(String schemaName, String tableName, String indexName);

    IndexMetadataDto getIndex(String schemaName, String tableName, String indexName,
            boolean includeTable, boolean checkIndexExists);

    List<IndexMetadataDto> getIndexesByTable(String schemaName, String tableName,
            boolean includeTable, boolean checkTableExists);

    IndexMetadataDto createIndex(NewIndexDto newIndexDto);
}
