package ma.ilias.dbmanagementbe;

import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import ma.ilias.dbmanagementbe.dao.entities.Permission;
import ma.ilias.dbmanagementbe.dao.entities.Role;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dao.repositories.PermissionRepository;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.enums.PermissionType;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.List;

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
            PasswordEncoder passwordEncoder) {
        return args -> {
            Permission permission1 = Permission.builder()
                    .schemaName("*")
                    .tableName("*")
                    .permissionType(PermissionType.READ)
                    .build();

            Permission permission2 = Permission.builder()
                    .schemaName("*")
                    .tableName("*")
                    .permissionType(PermissionType.WRITE)
                    .build();

            Permission permission3 = Permission.builder()
                    .schemaName("*")
                    .tableName("*")
                    .permissionType(PermissionType.CREATE)
                    .build();

            Permission permission4 = Permission.builder()
                    .schemaName("*")
                    .tableName("*")
                    .permissionType(PermissionType.DELETE)
                    .build();

            permissionRepository.saveAll(List.of(permission1, permission2, permission3, permission4));

            Role role1 = Role.builder()
                    .name("ADMIN")
                    .description("desc")
                    .isSystemRole(true)
                    .permissions(List.of(permission1, permission2, permission3, permission4))
                    .build();

            Role role2 = Role.builder()
                    .name("VIEWER")
                    .description("desc")
                    .isSystemRole(true)
                    .permissions(List.of(permission1))
                    .build();

            roleRepository.saveAll(List.of(role1, role2));

            AppUser appUser1 = AppUser.builder()
                    .username("ilias")
                    .email("ilias@example.com")
                    .passwordHash(passwordEncoder.encode("password"))
                    .active(true)
                    .roles(List.of(role1))
                    .build();

            AppUser appUser2 = AppUser.builder()
                    .username("messi")
                    .email("messi@example.com")
                    .passwordHash(passwordEncoder.encode("password"))
                    .active(true)
                    .roles(List.of(role2))
                    .build();

            appUserRepository.saveAll(List.of(appUser1, appUser2));
        };
    }
}
