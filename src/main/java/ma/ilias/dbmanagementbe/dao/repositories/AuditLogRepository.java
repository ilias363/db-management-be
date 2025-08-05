package ma.ilias.dbmanagementbe.dao.repositories;

import ma.ilias.dbmanagementbe.dao.entities.AuditLog;
import ma.ilias.dbmanagementbe.enums.ActionType;
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

    @Query("""
            SELECT a FROM AuditLog a LEFT JOIN FETCH a.user WHERE
            (:search IS NULL OR :search = '' OR
            LOWER(a.user.username) LIKE LOWER(CONCAT('%', :search, '%')) OR
            LOWER(a.schemaName) LIKE LOWER(CONCAT('%', :search, '%')) OR
            LOWER(a.tableName) LIKE LOWER(CONCAT('%', :search, '%')) OR
            LOWER(a.objectName) LIKE LOWER(CONCAT('%', :search, '%'))) AND
            (:userId IS NULL OR a.user.id = :userId) AND
            (:actionType IS NULL OR a.actionType = :actionType) AND
            (:successful IS NULL OR a.successful = :successful) AND
            (:after IS NULL OR a.auditTimestamp >= :after) AND
            (:before IS NULL OR a.auditTimestamp <= :before)
            """)
    Page<AuditLog> findAllWithFilters(@Param("search") String search,
                                      @Param("userId") Long userId,
                                      @Param("actionType") ActionType actionType,
                                      @Param("successful") Boolean successful,
                                      @Param("after") LocalDateTime after,
                                      @Param("before") LocalDateTime before,
                                      Pageable pageable);
}
