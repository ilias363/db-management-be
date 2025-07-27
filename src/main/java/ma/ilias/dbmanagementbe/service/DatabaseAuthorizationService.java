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
     * Check if the current user has permission to perform read operations on a specific schema/table/view
     */
    public void checkReadPermission(String schemaName, String objectName) {
        if (!AuthorizationUtils.hasPermission(
                PermissionType.READ,
                schemaName != null ? schemaName.trim().toLowerCase() : null,
                objectName != null ? objectName.trim().toLowerCase() : null)) {
            throw new InsufficientPermissionException(
                    String.format("Access denied: No read permission for %s.%s",
                            schemaName == null ? "*" : schemaName,
                            objectName == null ? "*" : objectName)
            );
        }
    }

    /**
     * Check if the current user has permission to perform write operations on a specific schema/table/view
     */
    public void checkWritePermission(String schemaName, String objectName) {
        if (!AuthorizationUtils.hasPermission(
                PermissionType.WRITE,
                schemaName != null ? schemaName.trim().toLowerCase() : null,
                objectName != null ? objectName.trim().toLowerCase() : null)) {
            throw new InsufficientPermissionException(
                    String.format("Access denied: No write permission for %s.%s",
                            schemaName == null ? "*" : schemaName,
                            objectName == null ? "*" : objectName)
            );
        }
    }

    /**
     * Check if the current user has permission to perform create operations on a specific schema/table/view
     */
    public void checkCreatePermission(String schemaName, String objectName) {
        if (!AuthorizationUtils.hasPermission(
                PermissionType.CREATE,
                schemaName != null ? schemaName.trim().toLowerCase() : null,
                objectName != null ? objectName.trim().toLowerCase() : null)) {
            throw new InsufficientPermissionException(
                    String.format("Access denied: No create permission for %s.%s",
                            schemaName == null ? "*" : schemaName,
                            objectName == null ? "*" : objectName)
            );
        }
    }

    /**
     * Check if the current user has permission to perform delete operations on a specific schema/table/view
     */
    public void checkDeletePermission(String schemaName, String objectName) {
        if (!AuthorizationUtils.hasPermission(
                PermissionType.DELETE,
                schemaName != null ? schemaName.trim().toLowerCase() : null,
                objectName != null ? objectName.trim().toLowerCase() : null)) {
            throw new InsufficientPermissionException(
                    String.format("Access denied: No delete permission for %s.%s",
                            schemaName == null ? "*" : schemaName,
                            objectName == null ? "*" : objectName)
            );
        }
    }
}
