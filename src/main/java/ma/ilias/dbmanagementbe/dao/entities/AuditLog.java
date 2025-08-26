package ma.ilias.dbmanagementbe.dao.entities;

import jakarta.persistence.*;
import lombok.*;
import ma.ilias.dbmanagementbe.enums.ActionType;
import org.hibernate.annotations.CreationTimestamp;

import java.time.LocalDateTime;

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
    @JoinColumn(name = "user_id") // nullable for failed auth actions
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private AppUser user;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private ActionType actionType;

    private String schemaName;
    private String tableName;
    private String objectName;

    @Column(length = 1000)
    private String actionDetails;

    @Column(nullable = false)
    private Boolean successful;

    @Column(length = 1000)
    private String errorMessage;

    @CreationTimestamp
    private LocalDateTime auditTimestamp;
}
