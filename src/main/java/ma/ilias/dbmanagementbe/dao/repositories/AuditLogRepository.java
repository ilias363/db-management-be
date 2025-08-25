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
import java.util.Optional;

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

    @Query("""
            SELECT u.username, COUNT(a.id), MAX(a.auditTimestamp)
            FROM AuditLog a
            JOIN a.user u
            WHERE a.auditTimestamp BETWEEN :startDate AND :endDate
            GROUP BY u.username
            ORDER BY COUNT(a.id) DESC
            """)
    List<Object[]> findTopUsersByActivity(@Param("startDate") LocalDateTime startDate,
                                          @Param("endDate") LocalDateTime endDate,
                                          Pageable pageable);

    @Query("""
            SELECT a.actionType
            FROM AuditLog a
            GROUP BY a.actionType
            ORDER BY COUNT(a.actionType) DESC
            """)
    List<ActionType> findMostCommonAction(Pageable pageable);

    @Query("""
            SELECT COUNT(a), MIN(a.auditTimestamp), MAX(a.auditTimestamp)
            FROM AuditLog a
            WHERE a.auditTimestamp IS NOT NULL
            """)
    Object[] findAuditCountAndDateRange();

    @Query("""
            SELECT
                a.auditTimestamp,
                EXTRACT(HOUR FROM a.auditTimestamp)
            FROM AuditLog a
            WHERE a.auditTimestamp BETWEEN :startDate AND :endDate
            """)
    List<Object[]> findAuditHeatmapData(@Param("startDate") LocalDateTime startDate,
                                        @Param("endDate") LocalDateTime endDate);

    @Query("""
            SELECT
                a.auditTimestamp,
                EXTRACT(HOUR FROM a.auditTimestamp)
            FROM AuditLog a
            WHERE a.auditTimestamp IS NOT NULL
            """)
    List<Object[]> findAuditHeatmapDataAllTime();

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

    long countByUser_Id(Long userId);

    long countByUser_IdAndSuccessful(Long userId, Boolean successful);

    long countByUserIdAndAuditTimestampAfter(Long userId, LocalDateTime auditTimestampAfter);

    @Query("""
            SELECT a.actionType
            FROM AuditLog a
            WHERE a.user.id = :userId
            GROUP BY a.actionType
            ORDER BY COUNT(a.actionType) DESC
            """)
    List<ActionType> findTopActionTypeByUserId(@Param("userId") Long userId, Pageable pageable);

    default Optional<String> findTopActionTypeByUserId(Long userId) {
        List<ActionType> result = findTopActionTypeByUserId(userId, Pageable.ofSize(1));
        return result.isEmpty() ? Optional.empty() : Optional.of(result.get(0).name());
    }

    @Query("""
            SELECT a.schemaName
            FROM AuditLog a
            WHERE a.user.id = :userId AND a.schemaName IS NOT NULL
            GROUP BY a.schemaName
            ORDER BY COUNT(a.schemaName) DESC
            """)
    List<String> findTopSchemaByUserId(@Param("userId") Long userId, Pageable pageable);

    default Optional<String> findTopSchemaByUserId(Long userId) {
        List<String> result = findTopSchemaByUserId(userId, Pageable.ofSize(1));
        return result.isEmpty() ? Optional.empty() : Optional.of(result.get(0));
    }

    @Query("""
            SELECT a.tableName
            FROM AuditLog a
            WHERE a.user.id = :userId AND a.tableName IS NOT NULL
            GROUP BY a.tableName
            ORDER BY COUNT(a.tableName) DESC
            """)
    List<String> findTopTableByUserId(@Param("userId") Long userId, Pageable pageable);

    default java.util.Optional<String> findTopTableByUserId(Long userId) {
        List<String> result = findTopTableByUserId(userId, Pageable.ofSize(1));
        return result.isEmpty() ? java.util.Optional.empty() : java.util.Optional.of(result.get(0));
    }

    @Query("SELECT COUNT(DISTINCT a.schemaName) FROM AuditLog a WHERE a.user.id = :userId AND a.schemaName IS NOT NULL")
    long countUniqueSchemasByUserId(@Param("userId") Long userId);

    @Query("SELECT COUNT(DISTINCT a.tableName) FROM AuditLog a WHERE a.user.id = :userId AND a.tableName IS NOT NULL")
    long countUniqueTablesByUserId(@Param("userId") Long userId);

    @Query("""
            SELECT a.actionType, COUNT(a.actionType)
            FROM AuditLog a
            WHERE a.user.id = :userId
            GROUP BY a.actionType
            ORDER BY COUNT(a.actionType) DESC
            """)
    List<Object[]> findUserActionBreakdown(@Param("userId") Long userId);

    @Query("""
            SELECT a.schemaName, COUNT(a.schemaName), MAX(a.auditTimestamp)
            FROM AuditLog a
            WHERE a.user.id = :userId AND a.schemaName IS NOT NULL
            GROUP BY a.schemaName
            ORDER BY COUNT(a.schemaName) DESC
            """)
    List<Object[]> findUserDatabaseAccess(@Param("userId") Long userId);

    @Query("""
            SELECT a.auditTimestamp, EXTRACT(HOUR FROM a.auditTimestamp)
            FROM AuditLog a
            WHERE a.user.id = :userId AND a.auditTimestamp IS NOT NULL
            """)
    List<Object[]> findUserAuditHeatmapData(@Param("userId") Long userId);
}
