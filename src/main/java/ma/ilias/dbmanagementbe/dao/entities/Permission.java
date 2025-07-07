package ma.ilias.dbmanagementbe.dao.entities;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.enums.PermissionType;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;

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

    private String tableName; // null for all tables in schema

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private PermissionType permissionType;

    @CreationTimestamp
    private LocalDateTime createdAt;

    @UpdateTimestamp
    private LocalDateTime updatedAt;

    @ManyToOne
    @JoinColumn(name = "role_id")
    private Role role;
}
