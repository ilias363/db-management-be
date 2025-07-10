package ma.ilias.dbmanagementbe.metadata.service.schema;

import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;

import java.util.List;

public interface SchemaMetadataService {
    Boolean schemaExists(String schemaName);
    Boolean isSystemSchemaByName(String schemaName);
    List<SchemaMetadataDto> getAllSchemas(Boolean includeSystemSchema);
}
