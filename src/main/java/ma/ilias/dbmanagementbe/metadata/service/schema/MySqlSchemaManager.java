package ma.ilias.dbmanagementbe.metadata.service.schema;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.metadata.dto.schema.NewSchemaDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.util.SqlSecurityUtils;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@AllArgsConstructor
@Transactional
public class MySqlSchemaManager implements SchemaService {

    private final JdbcTemplate jdbcTemplate;
    private final MetadataProviderService metadataProviderService;

    @Override
    public Boolean schemaExists(String schemaName) {
        return metadataProviderService.schemaExists(schemaName);
    }

    @Override
    public Boolean isSystemSchemaByName(String schemaName) {
        return metadataProviderService.isSystemSchemaByName(schemaName);
    }

    @Override
    public SchemaMetadataDto getSchemaByName(String schemaName, boolean includeTables, boolean checkSchemaExists) {
        return metadataProviderService.getSchemaByName(schemaName, includeTables, checkSchemaExists);
    }

    @Override
    public List<SchemaMetadataDto> getAllSchemas(Boolean includeSystemSchemas) {
        return metadataProviderService.getAllSchemas(includeSystemSchemas);
    }

    @Override
    public SchemaMetadataDto createSchema(NewSchemaDto newSchema) {
        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(newSchema.getSchemaName());

        jdbcTemplate.execute("CREATE DATABASE " + validatedSchemaName);

        return getSchemaByName(newSchema.getSchemaName(), false, false);
    }

    @Override
    public Boolean deleteSchema(String schemaName) {
        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(schemaName);

        if (isSystemSchemaByName(schemaName)) {
            throw new UnauthorizedActionException("Cannot delete system schema: " + schemaName);
        }

        if (!schemaExists(schemaName)) {
            throw new SchemaNotFoundException(schemaName);
        }

        jdbcTemplate.execute("DROP DATABASE " + validatedSchemaName);
        return !schemaExists(schemaName);
    }
}
