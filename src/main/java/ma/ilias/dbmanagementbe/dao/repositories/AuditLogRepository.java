package ma.ilias.dbmanagementbe.dao.repositories;

import ma.ilias.dbmanagementbe.dao.entities.AuditLog;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;

public interface AuditLogRepository extends JpaRepository<AuditLog, Long> {
    Page<AuditLog> findByUser_Id(Long userId, Pageable pageable);

    @Query("SELECT COUNT(a) FROM AuditLog a WHERE a.successful = true")
    long countSuccessfulAudits();

    @Query("SELECT COUNT(a) FROM AuditLog a WHERE a.successful = false")
    long countFailedAudits();

    @Query("SELECT COUNT(a) FROM AuditLog a WHERE a.auditTimestamp >= :since")
    long countAuditsSince(@Param("since") LocalDateTime since);

    @Query(value = """
            SELECT action_type FROM audit_logs
            GROUP BY action_type
            ORDER BY COUNT(action_type) DESC LIMIT 1
            """, nativeQuery = true)
    String findMostCommonActionString();

    @Query(value = """
            SELECT COUNT(*) / (DATEDIFF(CURDATE(), MIN(DATE(audit_timestamp))) + 1.0)
            FROM audit_logs
            WHERE audit_timestamp IS NOT NULL
            """, nativeQuery = true)
    Double calculateAverageActionsPerDay();
}
