package ma.ilias.dbmanagementbe.util;

import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import ma.ilias.dbmanagementbe.dao.entities.Permission;
import ma.ilias.dbmanagementbe.enums.PermissionType;
import ma.ilias.dbmanagementbe.enums.SystemRole;
import org.springframework.security.authorization.AuthorizationDecision;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Set;

@Component
public class AuthorizationUtils {
    public static AppUser getCurrentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() instanceof AppUser) {
            return (AppUser) authentication.getPrincipal();
        }
        return null;
    }

    /**
     * Check if the current user has an admin role or admin-equivalent permissions FOR DATABASE OPERATIONS
     * This includes the ADMIN system role or any custom role with admin-level DATABASE permissions
     * NOTE: This does NOT grant access to user/role/permission management - only database operations
     */
    public static boolean isAdmin() {
        AppUser currentUser = getCurrentUser();
        if (currentUser == null) {
            return false;
        }

        boolean hasSystemAdminRole = currentUser.getRoles().stream()
                .anyMatch(role -> SystemRole.ADMIN.name().equals(role.getName()));

        if (hasSystemAdminRole) {
            return true;
        }

        // Check if user has admin-equivalent permissions through custom roles
        return hasAdminLevelPermissions(currentUser);
    }

    /**
     * Check if the current user has a viewer role or viewer-equivalent permissions
     * This includes the VIEWER system role or any custom role with read-only permissions
     */
    public static boolean isViewer() {
        AppUser currentUser = getCurrentUser();
        if (currentUser == null) {
            return false;
        }

        // Check if user has the system VIEWER role
        boolean hasSystemViewerRole = currentUser.getRoles().stream()
                .anyMatch(role -> SystemRole.VIEWER.name().equals(role.getName()));

        if (hasSystemViewerRole) {
            return true;
        }

        // Check if user has viewer-equivalent permissions (read-only access)
        return hasViewerLevelPermissions(currentUser) && !hasAdminLevelPermissions(currentUser);
    }

    /**
     * Check if the current user has access to database operations.
     * Database operations are allowed for users with any database permissions
     */
    public static boolean hasDbAccess() {
        AppUser currentUser = getCurrentUser();
        if (currentUser == null) {
            return false;
        }

        return currentUser.getRoles().stream()
                .flatMap(role -> role.getPermissions().stream())
                .anyMatch(permission -> permission.getPermissionType() != null);
    }

    /**
     * Check if the current user has access to user management operations
     * This requires the ADMIN system role ONLY - custom roles cannot access user management
     */
    public static boolean hasUserManagementAccess() {
        AppUser currentUser = getCurrentUser();
        if (currentUser == null) {
            return false;
        }

        return currentUser.getRoles().stream()
                .anyMatch(role -> SystemRole.ADMIN.name().equals(role.getName()));
    }

    /**
     * Check if the current user has the system ADMIN role (not just admin-level database permissions)
     * This is used for system administration tasks like user/role/permission/audit management
     */
    public static boolean isSystemAdmin() {
        return hasUserManagementAccess();
    }

    /**
     * Check if the current user has write access to database
     * This includes CREATE, WRITE, DELETE permissions
     */
    public static boolean hasDbWriteAccess() {
        AppUser currentUser = getCurrentUser();
        if (currentUser == null) {
            return false;
        }

        // Check if user has any write permissions (CREATE, WRITE, DELETE)
        return currentUser.getRoles().stream()
                .flatMap(role -> role.getPermissions().stream())
                .anyMatch(permission ->
                        permission.getPermissionType() == PermissionType.CREATE ||
                                permission.getPermissionType() == PermissionType.WRITE ||
                                permission.getPermissionType() == PermissionType.DELETE
                );
    }

    /**
     * Check if the current user has read access to database
     * This includes READ permissions
     */
    public static boolean hasDbReadAccess() {
        AppUser currentUser = getCurrentUser();
        if (currentUser == null) {
            return false;
        }

        // Check if user has read permissions
        return currentUser.getRoles().stream()
                .flatMap(role -> role.getPermissions().stream())
                .anyMatch(permission -> permission.getPermissionType() == PermissionType.READ);
    }

    /**
     * Check if user has admin-level permissions
     * Admin-level means having all permission types (CREATE, READ, WRITE, DELETE) with global scope
     */
    private static boolean hasAdminLevelPermissions(AppUser user) {
        Set<Permission> allPermissions = user.getRoles().stream()
                .flatMap(role -> role.getPermissions().stream())
                .collect(java.util.stream.Collectors.toSet());

        boolean hasGlobalCreate = allPermissions.stream()
                .anyMatch(p -> p.getPermissionType() == PermissionType.CREATE &&
                        p.getSchemaName() == null && p.getTableName() == null && p.getViewName() == null);

        boolean hasGlobalRead = allPermissions.stream()
                .anyMatch(p -> p.getPermissionType() == PermissionType.READ &&
                        p.getSchemaName() == null && p.getTableName() == null && p.getViewName() == null);

        boolean hasGlobalWrite = allPermissions.stream()
                .anyMatch(p -> p.getPermissionType() == PermissionType.WRITE &&
                        p.getSchemaName() == null && p.getTableName() == null && p.getViewName() == null);

        boolean hasGlobalDelete = allPermissions.stream()
                .anyMatch(p -> p.getPermissionType() == PermissionType.DELETE &&
                        p.getSchemaName() == null && p.getTableName() == null && p.getViewName() == null);

        return hasGlobalCreate && hasGlobalRead && hasGlobalWrite && hasGlobalDelete;
    }

    /**
     * Check if user has viewer-level permissions
     * Viewer-level means having only READ permissions
     */
    private static boolean hasViewerLevelPermissions(AppUser user) {
        Set<Permission> allPermissions = user.getRoles().stream()
                .flatMap(role -> role.getPermissions().stream())
                .collect(java.util.stream.Collectors.toSet());

        return allPermissions.stream()
                .anyMatch(p -> p.getPermissionType() == PermissionType.READ &&
                        p.getSchemaName() == null && p.getTableName() == null && p.getViewName() == null);
    }

    /**
     * Check if the current user has a specific permission type for a given schema/table/view
     */
    public static boolean hasPermission(PermissionType permissionType, String schemaName, String objectName) {
        AppUser currentUser = getCurrentUser();
        if (currentUser == null) {
            return false;
        }

        return currentUser.getRoles().stream()
                .flatMap(role -> role.getPermissions().stream())
                .anyMatch(permission ->
                        permission.getPermissionType() == permissionType &&
                                permissionMatches(permission, schemaName, objectName)
                );
    }

    /**
     * Check if a permission matches the requested schema/table
     */
    private static boolean permissionMatches(Permission permission, String requestedSchema, String requestedObject) {
        // If permission schema is null, it applies to all schemas
        if (permission.getSchemaName() == null) {
            return true;
        }

        // If permission schema doesn't match requested schema, no match
        if (!permission.getSchemaName().equals(requestedSchema)) {
            return false;
        }

        // If permission table and view are null, it applies to all tables and views in the schema
        if (permission.getTableName() == null && permission.getViewName() == null) {
            return true;
        }

        if (permission.getTableName() != null) return permission.getTableName().equals(requestedObject);

        return permission.getViewName().equals(requestedObject);
    }

    /**
     * Get accessible schemas based on user permissions
     */
    public static List<String> getAccessibleSchemas() {
        AppUser currentUser = getCurrentUser();
        if (currentUser == null) {
            return Collections.emptyList();
        }

        return currentUser.getRoles().stream()
                .flatMap(role -> role.getPermissions().stream())
                .filter(permission ->
                        permission.getPermissionType() == PermissionType.READ &&
                                permission.getSchemaName() != null
                )
                .map(permission -> permission.getSchemaName().toLowerCase())
                .toList();
    }

    /**
     * Get accessible tables in a schema based on user permissions
     */
    public static List<String> getAccessibleTablesInSchema(String schemaName) {
        AppUser currentUser = getCurrentUser();
        if (currentUser == null) {
            return Collections.emptyList();
        }

        return currentUser.getRoles().stream()
                .flatMap(role -> role.getPermissions().stream())
                .filter(permission ->
                        permission.getPermissionType() == PermissionType.READ &&
                                permission.getSchemaName().equalsIgnoreCase(schemaName) &&
                                permission.getTableName() != null
                )
                .map(permission -> permission.getTableName().toLowerCase())
                .toList();
    }

    /**
     * Get accessible views in a schema based on user permissions
     */
    public static List<String> getAccessibleViewsInSchema(String schemaName) {
        AppUser currentUser = getCurrentUser();
        if (currentUser == null) {
            return Collections.emptyList();
        }

        return currentUser.getRoles().stream()
                .flatMap(role -> role.getPermissions().stream())
                .filter(permission ->
                        permission.getPermissionType() == PermissionType.READ &&
                                permission.getSchemaName().equalsIgnoreCase(schemaName) &&
                                permission.getViewName() != null
                )
                .map(permission -> permission.getTableName().toLowerCase())
                .toList();
    }

    // Spring Security Authorization Decision Methods

    /**
     * Create authorization decision for user management access
     * Only system ADMIN role is allowed - custom roles are NOT permitted
     */
    public static AuthorizationDecision createUserManagementDecision() {
        return new AuthorizationDecision(hasUserManagementAccess());
    }

    /**
     * Create authorization decision for general database access
     */
    public static AuthorizationDecision createDbAccessDecision() {
        return new AuthorizationDecision(hasDbAccess());
    }

    /**
     * Create authorization decision for database read access
     */
    public static AuthorizationDecision createDbReadDecision() {
        return new AuthorizationDecision(hasDbReadAccess());
    }

    /**
     * Create authorization decision for database write access
     */
    public static AuthorizationDecision createDbWriteDecision() {
        return new AuthorizationDecision(hasDbWriteAccess());
    }
}
