package ma.ilias.dbmanagementbe.service;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import ma.ilias.dbmanagementbe.dao.entities.Permission;
import ma.ilias.dbmanagementbe.dao.entities.Role;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dao.repositories.PermissionRepository;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.enums.PermissionType;
import ma.ilias.dbmanagementbe.enums.SystemRole;
import org.springframework.boot.CommandLineRunner;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashSet;
import java.util.Set;

@Service
@AllArgsConstructor
@Slf4j
@Transactional
public class SystemRoleAndAdminUserInitService implements CommandLineRunner {

    private final RoleRepository roleRepository;
    private final PermissionRepository permissionRepository;
    private final AppUserRepository appUserRepository;
    private final PasswordEncoder passwordEncoder;

    @Override
    public void run(String... args) throws Exception {
        initializeSystemRolesIfNotExist();
        intitializeAdminUserIfNotExists();
    }

    private void initializeSystemRolesIfNotExist() {
        if (roleRepository.findByName(SystemRole.ADMIN.name()).isPresent()) {
            log.info("System role 'admin' already exist, skipping initialization");
        } else {
            log.info("Initializing system role 'admin'...");

            // Create ADMIN role with all permissions
            createAdminRole();

            log.info("System role 'admin' initialized successfully");
        }

        if (roleRepository.findByName(SystemRole.VIEWER.name()).isPresent()) {
            log.info("System role 'viewer' already exist, skipping initialization");
        } else {
            log.info("Initializing system role 'viewer'...");

            // Create VIEWER role with read-only permissions
            createViewerRole();

            log.info("System role 'viewer' initialized successfully");
        }

    }

    private void createAdminRole() {
        Role adminRole = roleRepository.findByName(SystemRole.ADMIN.name())
                .orElse(Role.builder()
                        .name(SystemRole.ADMIN.name())
                        .description("Administrator role with full access to all system features")
                        .isSystemRole(true)
                        .permissions(new HashSet<>())
                        .build());

        adminRole = roleRepository.save(adminRole);

        // Admin has all database permissions
        Set<Permission> adminPermissions = createAllDatabasePermissions(adminRole);
        adminRole.setPermissions(adminPermissions);

        roleRepository.save(adminRole);
        log.info("ADMIN role created");
    }

    private void createViewerRole() {
        Role viewerRole = roleRepository.findByName(SystemRole.VIEWER.name())
                .orElse(Role.builder()
                        .name(SystemRole.VIEWER.name())
                        .description("Viewer role with read-only access to database")
                        .isSystemRole(true)
                        .permissions(new HashSet<>())
                        .build());

        viewerRole = roleRepository.save(viewerRole);

        // Viewer has only read permissions
        Set<Permission> viewerPermissions = createReadOnlyDatabasePermissions(viewerRole);
        viewerRole.setPermissions(viewerPermissions);

        roleRepository.save(viewerRole);
        log.info("VIEWER role created");
    }

    private Set<Permission> createAllDatabasePermissions(Role role) {
        Set<Permission> permissions = new HashSet<>();

        // Create permissions for all database operations (READ, WRITE, DELETE, CREATE)
        // null schema and table means access to all schemas and tables
        for (PermissionType permissionType : PermissionType.values()) {
            Permission permission = Permission.builder()
                    .schemaName(null) // null means all schemas
                    .tableName(null)  // null means all tables
                    .permissionType(permissionType)
                    .role(role)
                    .build();
            Permission savedPermission = permissionRepository.save(permission);
            permissions.add(savedPermission);
        }

        return permissions;
    }

    private Set<Permission> createReadOnlyDatabasePermissions(Role role) {
        Set<Permission> permissions = new HashSet<>();

        // Create only READ permission for viewer
        Permission readPermission = Permission.builder()
                .schemaName(null) // null means all schemas
                .tableName(null)  // null means all tables
                .permissionType(PermissionType.READ)
                .role(role)
                .build();
        Permission savedPermission = permissionRepository.save(readPermission);
        permissions.add(savedPermission);

        return permissions;
    }

    private void intitializeAdminUserIfNotExists() {
        log.info("Initializing admin user if not exists...");

        // Check if any user with ADMIN role exists
        Role adminRole = roleRepository.findByName(SystemRole.ADMIN.name())
                .orElseThrow(() -> new RuntimeException("ADMIN role not found. Please ensure system roles are initialized first."));

        // Check if there's already a user with admin role
        boolean adminUserExists = appUserRepository.findAll().stream()
                .anyMatch(user -> user.getRoles().contains(adminRole));

        if (adminUserExists) {
            log.info("Admin user already exists, skipping initialization");
            return;
        }

        AppUser adminUser = AppUser.builder()
                .username("admin")
                .password(passwordEncoder.encode("admin123")) // Default password
                .active(true)
                .roles(Set.of(adminRole))
                .build();

        appUserRepository.save(adminUser);

        log.info("Default admin user created successfully!");
        log.info("Username: admin");
        log.info("Password: admin123");
        log.warn("Please change the default password after first login for security!");
    }
}
