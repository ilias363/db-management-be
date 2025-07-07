package ma.ilias.dbmanagementbe;

import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import ma.ilias.dbmanagementbe.dao.entities.AuditLog;
import ma.ilias.dbmanagementbe.dao.entities.Permission;
import ma.ilias.dbmanagementbe.dao.entities.Role;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dao.repositories.AuditLogRepository;
import ma.ilias.dbmanagementbe.dao.repositories.PermissionRepository;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.enums.PermissionType;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.List;
import java.util.Set;

@SpringBootApplication
public class DbManagementBeApplication {

    public static void main(String[] args) {
        SpringApplication.run(DbManagementBeApplication.class, args);
    }

    @Bean
    CommandLineRunner commandLineRunner(
            AppUserRepository appUserRepository,
            RoleRepository roleRepository,
            PermissionRepository permissionRepository,
            AuditLogRepository auditLogRepository,
            PasswordEncoder passwordEncoder) {
        return args -> {

            Role role1 = Role.builder()
                    .name("ADMIN")
                    .description("desc")
                    .isSystemRole(true)
                    .build();

            Role role2 = Role.builder()
                    .name("VIEWER")
                    .description("desc")
                    .isSystemRole(true)
                    .build();

            roleRepository.saveAll(List.of(role1, role2));

            Permission permission1 = Permission.builder()
                    .permissionType(PermissionType.READ)
                    .role(role1)
                    .build();
            Permission permission2 = Permission.builder()
                    .permissionType(PermissionType.WRITE)
                    .role(role1)
                    .build();
            Permission permission3 = Permission.builder()
                    .permissionType(PermissionType.CREATE)
                    .role(role1)
                    .build();
            Permission permission4 = Permission.builder()
                    .permissionType(PermissionType.DELETE)
                    .role(role1)
                    .build();
            Permission permission5 = Permission.builder()
                    .permissionType(PermissionType.READ)
                    .role(role2)
                    .build();

            permissionRepository.saveAll(List.of(
                    permission1, permission2,permission3, permission4, permission5
            ));

            AppUser appUser1 = AppUser.builder()
                    .username("theadmin")
                    .password(passwordEncoder.encode("password"))
                    .active(true)
                    .roles(Set.of(role1))
                    .build();

            AppUser appUser2 = AppUser.builder()
                    .username("theviewer")
                    .password(passwordEncoder.encode("password"))
                    .active(true)
                    .roles(Set.of(role2))
                    .build();

            appUserRepository.saveAll(List.of(appUser1, appUser2));

            AuditLog auditLog1 = AuditLog.builder()
                    .user(appUser1)
                    .actionType(ActionType.CREATE_TABLE)
                    .schemaName("public")
                    .tableName("customers")
                    .successful(true)
                    .build();

            AuditLog auditLog2 = AuditLog.builder()
                    .user(appUser1)
                    .actionType(ActionType.UPDATE_TABLE)
                    .schemaName("public")
                    .tableName("customers")
                    .actionDetails("SET name = 'new name' WHERE id = 1")
                    .successful(true)
                    .build();

            AuditLog auditLog3 = AuditLog.builder()
                    .user(appUser1)
                    .actionType(ActionType.DELETE_TABLE)
                    .schemaName("public")
                    .tableName("orders")
                    .successful(true)
                    .build();

            AuditLog auditLog4 = AuditLog.builder()
                    .user(appUser1)
                    .actionType(ActionType.CREATE_INDEX)
                    .schemaName("public")
                    .tableName("customers")
                    .objectName("idx_customer_name")
                    .successful(true)
                    .build();

            AuditLog auditLog5 = AuditLog.builder()
                    .user(appUser2)
                    .actionType(ActionType.DELETE_TABLE)
                    .schemaName("public")
                    .tableName("products")
                    .successful(false)
                    .errorMessage("User does not have permission to delete tables.")
                    .build();

            AuditLog auditLog6 = AuditLog.builder()
                    .user(appUser2)
                    .actionType(ActionType.CREATE_RECORD)
                    .schemaName("public")
                    .tableName("customers")
                    .successful(true)
                    .build();

            AuditLog auditLog7 = AuditLog.builder()
                    .user(appUser2)
                    .actionType(ActionType.CREATE_RECORD)
                    .schemaName("public")
                    .tableName("products")
                    .successful(false)
                    .errorMessage("Table does not exist.")
                    .build();

            auditLogRepository.saveAll(List.of(auditLog1, auditLog2, auditLog3, auditLog4, auditLog5, auditLog6, auditLog7));
        };
    }
}
