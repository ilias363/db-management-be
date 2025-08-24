package ma.ilias.dbmanagementbe.dao.repositories;

import ma.ilias.dbmanagementbe.dao.entities.AuditLog;
import ma.ilias.dbmanagementbe.enums.ActionType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;

public interface AuditLogRepository extends JpaRepository<AuditLog, Long> {
    Page<AuditLog> findByUser_Id(Long userId, Pageable pageable);

    @Query("SELECT COUNT(a) FROM AuditLog a WHERE a.successful = true")
    long countSuccessfulAudits();

    @Query("SELECT COUNT(a) FROM AuditLog a WHERE a.successful = false")
    long countFailedAudits();

    @Query("SELECT COUNT(a) FROM AuditLog a WHERE a.auditTimestamp >= :since")
    long countAuditsSince(@Param("since") LocalDateTime since);

    @Query("SELECT COUNT(DISTINCT a.user.id) FROM AuditLog a WHERE a.auditTimestamp >= :startDate AND a.auditTimestamp < :endDate")
    long countUniqueUsersInPeriod(@Param("startDate") LocalDateTime startDate, @Param("endDate") LocalDateTime endDate);

    @Query("SELECT COUNT(a) FROM AuditLog a WHERE a.auditTimestamp >= :startDate AND a.auditTimestamp < :endDate")
    long countAuditsInPeriod(@Param("startDate") LocalDateTime startDate, @Param("endDate") LocalDateTime endDate);

    @Query("SELECT COUNT(a) FROM AuditLog a WHERE a.auditTimestamp >= :startDate AND a.auditTimestamp < :endDate AND a.successful = true")
    long countSuccessfulAuditsInPeriod(@Param("startDate") LocalDateTime startDate, @Param("endDate") LocalDateTime endDate);

    @Query(value = """
            SELECT u.username, COUNT(a.id) as activity_count, MAX(a.audit_timestamp) as last_active
            FROM app_users u
            LEFT JOIN audit_logs a ON u.id = a.user_id
            WHERE a.audit_timestamp BETWEEN :startDate AND :endDate
            GROUP BY u.username
            ORDER BY activity_count DESC
            LIMIT :limit
            """, nativeQuery = true)
    List<Object[]> findTopUsersByActivity(@Param("startDate") LocalDateTime startDate,
                                          @Param("endDate") LocalDateTime endDate,
                                          @Param("limit") Integer limit);

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
