package ma.ilias.dbmanagementbe.metadata.service.schema;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.enums.PermissionType;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.metadata.dto.schema.NewSchemaDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.service.DatabaseAuthorizationService;
import ma.ilias.dbmanagementbe.util.AuthorizationUtils;
import ma.ilias.dbmanagementbe.util.SqlSecurityUtils;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@AllArgsConstructor
@Transactional
public class MySqlSchemaManager implements SchemaService {

    private final JdbcTemplate jdbcTemplate;
    private final MetadataProviderService metadataProviderService;
    private final DatabaseAuthorizationService databaseAuthorizationService;

    @Override
    public Boolean schemaExists(String schemaName) {
        return metadataProviderService.schemaExists(schemaName);
    }

    @Override
    public Boolean isSystemSchemaByName(String schemaName) {
        return metadataProviderService.isSystemSchemaByName(schemaName);
    }

    @Override
    public SchemaMetadataDto getSchemaByName(String schemaName, boolean includeTables,
                                             boolean includeViews, boolean checkSchemaExists) {
        return getSchemaByName(schemaName, includeTables, includeViews, checkSchemaExists, false);
    }

    @Override
    public SchemaMetadataDto getSchemaByName(String schemaName, boolean includeTables, boolean includeViews,
                                             boolean checkSchemaExists, boolean checkAuthorization) {
        String normalizedSchemaName = schemaName != null ? schemaName.trim().toLowerCase() : null;

        if (AuthorizationUtils.hasPermission(PermissionType.READ, normalizedSchemaName, null)) {
            return metadataProviderService.getSchemaByName(schemaName, includeTables, includeViews, checkSchemaExists);
        }

        List<String> accessibleTables = AuthorizationUtils.getAccessibleTablesInSchema(schemaName);
        List<String> accessibleViews = AuthorizationUtils.getAccessibleViewsInSchema(schemaName);

        return metadataProviderService.getSchemaByName(schemaName, accessibleTables, accessibleViews,
                includeTables, includeViews, checkSchemaExists);
    }

    @Override
    public List<SchemaMetadataDto> getAllSchemas(Boolean includeSystemSchemas) {
        if (AuthorizationUtils.hasPermission(PermissionType.READ, null, null)) {
            return metadataProviderService.getAllSchemas(includeSystemSchemas, false, false);
        }

        List<String> accessibleSchemas = AuthorizationUtils.getAccessibleSchemas();
        Map<String, Map<String, List<String>>> accessibleSchemasInfo = new HashMap<>();

        accessibleSchemas.forEach(schema ->
                accessibleSchemasInfo.put(schema, Map.of(
                        "tables", Collections.emptyList(),
                        "views", Collections.emptyList()
                )));

        return metadataProviderService.getAllSchemas(
                accessibleSchemasInfo, includeSystemSchemas, false, false);
    }

    @Override
    public SchemaMetadataDto createSchema(NewSchemaDto newSchema) {
        databaseAuthorizationService.checkCreatePermission(newSchema.getSchemaName(), null);

        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(newSchema.getSchemaName());

        jdbcTemplate.execute("CREATE DATABASE " + validatedSchemaName);

        return getSchemaByName(newSchema.getSchemaName(), false, false, false);
    }

    @Override
    public Boolean deleteSchema(String schemaName) {
        databaseAuthorizationService.checkDeletePermission(schemaName, null);

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

    /**
     * Helper method to get list of tables in a schema that the current user has read access to
     */
    private List<String> getUserAccessibleTablesInSchema(String schemaName) {
        String sql = "SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = ? AND TABLE_TYPE = 'BASE TABLE'";
        List<String> allTables = jdbcTemplate.queryForList(sql, String.class, schemaName);

        // Filter tables based on user's read permissions
        return allTables.stream()
                .map(table -> table.toLowerCase())
                .filter(tableName -> AuthorizationUtils.hasPermission(
                        PermissionType.READ,
                        schemaName,
                        tableName))
                .toList();
    }
}
