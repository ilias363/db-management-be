package ma.ilias.dbmanagementbe.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.enums.PermissionType;
import ma.ilias.dbmanagementbe.exception.InsufficientPermissionException;
import ma.ilias.dbmanagementbe.util.AuthorizationUtils;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class DatabaseAuthorizationService {

    /**
     * Check if the current user has permission to perform read operations on a specific schema/table
     */
    public void checkReadPermission(String schemaName, String tableName) {
        if (!AuthorizationUtils.hasPermission(
                PermissionType.READ,
                schemaName != null ? schemaName.trim().toLowerCase() : null,
                tableName != null ? tableName.trim().toLowerCase() : null)) {
            throw new InsufficientPermissionException(
                    String.format("Access denied: No read permission for %s.%s",
                            schemaName == null ? "*" : schemaName,
                            tableName == null ? "*" : tableName)
            );
        }
    }

    /**
     * Check if the current user has permission to perform write operations on a specific schema/table
     */
    public void checkWritePermission(String schemaName, String tableName) {
        if (!AuthorizationUtils.hasPermission(
                PermissionType.WRITE,
                schemaName != null ? schemaName.trim().toLowerCase() : null,
                tableName != null ? tableName.trim().toLowerCase() : null)) {
            throw new InsufficientPermissionException(
                    String.format("Access denied: No write permission for %s.%s",
                            schemaName == null ? "*" : schemaName,
                            tableName == null ? "*" : tableName)
            );
        }
    }

    /**
     * Check if the current user has permission to perform create operations on a specific schema/table
     */
    public void checkCreatePermission(String schemaName, String tableName) {
        if (!AuthorizationUtils.hasPermission(
                PermissionType.CREATE,
                schemaName != null ? schemaName.trim().toLowerCase() : null,
                tableName != null ? tableName.trim().toLowerCase() : null)) {
            throw new InsufficientPermissionException(
                    String.format("Access denied: No create permission for %s.%s",
                            schemaName == null ? "*" : schemaName,
                            tableName == null ? "*" : tableName)
            );
        }
    }

    /**
     * Check if the current user has permission to perform delete operations on a specific schema/table
     */
    public void checkDeletePermission(String schemaName, String tableName) {
        if (!AuthorizationUtils.hasPermission(
                PermissionType.DELETE,
                schemaName != null ? schemaName.trim().toLowerCase() : null,
                tableName != null ? tableName.trim().toLowerCase() : null)) {
            throw new InsufficientPermissionException(
                    String.format("Access denied: No delete permission for %s.%s",
                            schemaName == null ? "*" : schemaName,
                            tableName == null ? "*" : tableName)
            );
        }
    }

    /**
     * Check if the current user has any database access
     */
    public void checkDatabaseAccess() {
        if (!AuthorizationUtils.hasDbAccess()) {
            throw new InsufficientPermissionException("Access denied: No database access permissions");
        }
    }

    /**
     * Check if the current user has general write access to database
     */
    public void checkDatabaseWriteAccess() {
        if (!AuthorizationUtils.hasDbWriteAccess()) {
            throw new InsufficientPermissionException("Access denied: No database write permissions");
        }
    }
}
