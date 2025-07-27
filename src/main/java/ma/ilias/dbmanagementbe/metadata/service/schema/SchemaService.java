package ma.ilias.dbmanagementbe.metadata.service.schema;

import ma.ilias.dbmanagementbe.metadata.dto.schema.NewSchemaDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;

import java.util.List;

public interface SchemaService {
    Boolean schemaExists(String schemaName);

    Boolean isSystemSchemaByName(String schemaName);

    List<SchemaMetadataDto> getAllSchemas(Boolean includeSystemSchema);

    SchemaMetadataDto getSchemaByName(String schemaName, boolean includeTables, boolean includeViews,
                                      boolean checkSchemaExists, boolean checkAuthorization);

    SchemaMetadataDto getSchemaByName(String schemaName, boolean includeTables,
                                      boolean includeViews, boolean checkSchemaExists);

    SchemaMetadataDto createSchema(NewSchemaDto nweSchema);

    Boolean deleteSchema(String schemaName);
}
