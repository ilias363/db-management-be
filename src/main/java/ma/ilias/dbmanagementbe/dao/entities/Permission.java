package ma.ilias.dbmanagementbe.dao.entities;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.persistence.*;
import lombok.*;
import ma.ilias.dbmanagementbe.enums.PermissionType;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;

@Entity
@Table(name = "permissions")
@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class Permission {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String schemaName; // null for all schemas

    // null for both table and view means permission for all tables and views
    private String tableName;
    private String viewName;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private PermissionType permissionType;

    @CreationTimestamp
    private LocalDateTime createdAt;

    @UpdateTimestamp
    private LocalDateTime updatedAt;

    @ManyToOne
    @JoinColumn(name = "role_id")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    @JsonIgnoreProperties("permissions")
    private Role role;
}
