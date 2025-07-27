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

import java.util.List;

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

        // Check if user has schema-level read permission
        boolean hasSchemaPermission = AuthorizationUtils.hasPermission(PermissionType.READ, normalizedSchemaName, null);

        if (!hasSchemaPermission) {
            // If no schema-level permission, check if user has permission on any tables in this schema
            List<String> accessibleTables = getUserAccessibleTablesInSchema(normalizedSchemaName);
            if (accessibleTables.isEmpty()) {
                // User has no access to this schema or any tables within it
                databaseAuthorizationService.checkReadPermission(schemaName, null); // This will throw an exception
            }
        }

        SchemaMetadataDto schema = metadataProviderService.getSchemaByName(
                schemaName, includeTables, includeViews, checkSchemaExists);

        // If user doesn't have schema-level permission, filter the tables to only show accessible ones
        if (!hasSchemaPermission && schema.getTables() != null) {
            List<String> accessibleTables = getUserAccessibleTablesInSchema(normalizedSchemaName);
            schema.setTables(schema.getTables().stream()
                    .filter(table -> accessibleTables.contains(table.getTableName().toLowerCase()))
                    .toList());
        }

        return schema;
    }

    @Override
    public List<SchemaMetadataDto> getAllSchemas(Boolean includeSystemSchemas) {
        List<SchemaMetadataDto> allSchemas = metadataProviderService.getAllSchemas(includeSystemSchemas);

        // Filter schemas based on read permissions and apply table filtering to each schema
        return allSchemas.stream()
                .filter(schema -> {
                    String schemaName = schema.getSchemaName();

                    // Check if user has schema-level read permission
                    if (AuthorizationUtils.hasPermission(PermissionType.READ, schemaName, null)) {
                        return true;
                    }

                    List<String> accessibleTables = getUserAccessibleTablesInSchema(schemaName);
                    return !accessibleTables.isEmpty(); // Include schema only if user has access to at least one table
                })
                .peek(schema -> {
                    String schemaName = schema.getSchemaName();

                    // Check if user has schema-level read permission
                    boolean hasSchemaPermission = AuthorizationUtils.hasPermission(
                            PermissionType.READ, schemaName, null);

                    // If user doesn't have schema-level permission, filter the tables to only show accessible ones
                    if (!hasSchemaPermission && schema.getTables() != null) {
                        List<String> accessibleTables = getUserAccessibleTablesInSchema(schemaName);
                        schema.setTables(schema.getTables().stream()
                                .filter(table -> accessibleTables.contains(table.getTableName().toLowerCase()))
                                .toList());
                    }

                })
                .toList();
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
