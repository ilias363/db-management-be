package ma.ilias.dbmanagementbe.dao.entities;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

@Entity
@Table(name = "roles")
@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class Role {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(unique = true, nullable = false)
    private String name; // ADMIN, VIEWER, CUSTOM_ROLE_NAME

    private String description;

    @Column(nullable = false)
    private Boolean isSystemRole; // true for ADMIN/VIEWER

    @CreationTimestamp
    private LocalDateTime createdAt;

    @UpdateTimestamp
    private LocalDateTime updatedAt;

    @OneToMany(mappedBy = "role", cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.EAGER)
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    @JsonIgnoreProperties("role")
    @Builder.Default
    private Set<Permission> permissions = new HashSet<>();

    public Set<Permission> getPermissionsWithoutRole() {
        if (permissions != null) {
            return permissions.stream()
                    .map(permission -> Permission.builder()
                            .id(permission.getId())
                            .schemaName(permission.getSchemaName())
                            .tableName(permission.getTableName())
                            .viewName(permission.getViewName())
                            .permissionType(permission.getPermissionType())
                            .build())
                    .collect(Collectors.toSet());
        }
        return new HashSet<>();
    }
}
