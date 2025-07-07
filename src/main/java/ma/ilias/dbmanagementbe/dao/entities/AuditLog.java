package ma.ilias.dbmanagementbe.dao.entities;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ActionType;
import org.hibernate.annotations.CreationTimestamp;

import java.time.LocalDateTime;

import lombok.EqualsAndHashCode;
import lombok.ToString;

@Entity
@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@Table(name = "audit_logs")
public class AuditLog {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false)
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private AppUser user;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private ActionType actionType;

    private String schemaName;
    private String tableName;
    private String objectName;

    private String actionDetails;

    @Column(nullable = false)
    private Boolean successful;

    private String errorMessage;

    @CreationTimestamp
    private LocalDateTime auditTimestamp;
}