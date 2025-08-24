package ma.ilias.dbmanagementbe.analytics.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ma.ilias.dbmanagementbe.analytics.dto.AuditActivityDto;
import ma.ilias.dbmanagementbe.analytics.dto.DashboardStatsDto;
import ma.ilias.dbmanagementbe.analytics.dto.DatabaseUsageDto;
import ma.ilias.dbmanagementbe.analytics.dto.UserActivityDto;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dao.repositories.AuditLogRepository;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class MySqlAnalyticsManager implements AnalyticsService {

    private final AppUserRepository appUserRepository;
    private final AuditLogRepository auditLogRepository;
    private final RoleRepository roleRepository;
    private final JdbcTemplate jdbcTemplate;

    @Override
    public DashboardStatsDto getDashboardStats(boolean includeSystem) {
        long totalUsers = appUserRepository.count();
        long activeUsers = appUserRepository.countActiveUsers();
        long totalRoles = roleRepository.count();
        long totalAudits = auditLogRepository.count();
        long recentAudits = auditLogRepository.countAuditsSince(LocalDateTime.now().minusDays(7));

        return DashboardStatsDto.builder()
                .totalUsers(totalUsers)
                .totalActiveUsers(activeUsers)
                .totalRoles(totalRoles)
                .totalAudits(totalAudits)
                .totalSchemas(getDatabaseSchemaCount(includeSystem))
                .totalTables(getDatabaseTableCount(includeSystem))
                .totalViews(getDatabaseViewCount(includeSystem))
                .totalRecords(getDatabaseRecordCount(includeSystem))
                .last7DaysActivity(recentAudits)
                .build();
    }

    @Override
    public List<UserActivityDto> getUserActivity(LocalDateTime startDate, LocalDateTime endDate, String period) {
        List<UserActivityDto> activity = new ArrayList<>();
        LocalDateTime current = startDate;

        while (current.isBefore(endDate)) {
            LocalDateTime nextPeriod = incrementPeriod(current, period);

            long activeUsers = auditLogRepository.countUniqueUsersInPeriod(current, nextPeriod);
            long totalUsers = appUserRepository.countUsersBeforeDate(nextPeriod);
            long newUsers = appUserRepository.countUsersCreatedInPeriod(current, nextPeriod);

            activity.add(UserActivityDto.builder()
                    .date(current.toString())
                    .activeUsers(activeUsers)
                    .totalUsers(totalUsers)
                    .newUsers(newUsers)
                    .build());

            current = nextPeriod;
        }

        return activity;
    }

    @Override
    public List<DatabaseUsageDto> getDatabaseUsage(boolean includeSystem) {
        String sql = "SELECT " +
                "table_schema as schema_name, " +
                "COUNT(*) as table_count, " +
                "IFNULL(SUM(table_rows), 0) as record_count, " +
                "IFNULL(SUM(data_length + index_length) / 1024 / 1024, 0) as size_mb, " +
                "MAX(update_time) as last_accessed " +
                "FROM information_schema.tables WHERE table_type = 'BASE TABLE'" +
                (includeSystem ? "table_schema NOT IN ('information_schema', 'mysql', 'performance_schema', 'sys') " : "") +
                "GROUP BY table_schema " +
                "ORDER BY size_mb DESC";

        return jdbcTemplate.query(sql,
                (rs, rowNum) -> DatabaseUsageDto.builder()
                        .schemaName(rs.getString("schema_name"))
                        .tableCount((long) rs.getInt("table_count"))
                        .recordCount(rs.getLong("record_count"))
                        .size((long) rs.getDouble("size_mb"))
                        .lastModified(rs.getTimestamp("last_accessed") != null ?
                                rs.getTimestamp("last_accessed").toString() :
                                "unknown")
                        .build()
        );
    }

    @Override
    public List<AuditActivityDto> getAuditActivity(LocalDateTime startDate, LocalDateTime endDate, String period) {
        List<AuditActivityDto> activity = new ArrayList<>();
        LocalDateTime current = startDate;

        while (current.isBefore(endDate)) {
            LocalDateTime nextPeriod = incrementPeriod(current, period);

            Long totalActions = auditLogRepository.countAuditsInPeriod(current, nextPeriod);
            Long successfulActions = auditLogRepository.countSuccessfulAuditsInPeriod(current, nextPeriod);
            Long failedActions = totalActions - successfulActions;
            Long activeUsers = auditLogRepository.countUniqueUsersInPeriod(current, nextPeriod);

            activity.add(AuditActivityDto.builder()
                    .date(current.toString())
                    .totalActions(totalActions)
                    .successfulActions(successfulActions)
                    .failedActions(failedActions)
                    .activeUsers(activeUsers)
                    .build());

            current = nextPeriod;
        }

        return activity;
    }

    private long getDatabaseSchemaCount(boolean includeSystem) {
        String schemaCountSql = "SELECT COUNT(*) FROM INFORMATION_SCHEMA.SCHEMATA" +
                (includeSystem ? "" : " WHERE SCHEMA_NAME NOT IN ('mysql','sys','information_schema','performance_schema')");
        Long totalSchemas = jdbcTemplate.queryForObject(schemaCountSql, Long.class);
        return totalSchemas != null ? totalSchemas : 0L;
    }

    private long getDatabaseTableCount(boolean includeSystem) {
        String tableCountSql = "SELECT COUNT(*) FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE = 'BASE TABLE'" +
                (includeSystem ? "" : " AND TABLE_SCHEMA NOT IN ('mysql','sys','information_schema','performance_schema')");
        Long totalTables = jdbcTemplate.queryForObject(tableCountSql, Long.class);
        return totalTables != null ? totalTables : 0L;
    }

    private long getDatabaseViewCount(boolean includeSystem) {
        String viewCountSql = "SELECT COUNT(*) FROM INFORMATION_SCHEMA.VIEWS" +
                (includeSystem ? "" : " WHERE TABLE_SCHEMA NOT IN ('mysql','sys','information_schema','performance_schema')");
        Long totalViews = jdbcTemplate.queryForObject(viewCountSql, Long.class);
        return totalViews != null ? totalViews : 0L;
    }

    private long getDatabaseRecordCount(boolean includeSystem) {
        String recordCountSql = "SELECT SUM(TABLE_ROWS) FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE = 'BASE TABLE'" +
                (includeSystem ? "" : " AND TABLE_SCHEMA NOT IN ('mysql','sys','information_schema','performance_schema')");
        Long totalRecords = jdbcTemplate.queryForObject(recordCountSql, Long.class);
        return totalRecords != null ? totalRecords : 0L;
    }

    private LocalDateTime incrementPeriod(LocalDateTime dateTime, String period) {
        return switch (period.toLowerCase()) {
            case "hour" -> dateTime.plusHours(1);
            case "day" -> dateTime.plusDays(1);
            case "week" -> dateTime.plusWeeks(1);
            case "month" -> dateTime.plusMonths(1);
            default -> dateTime.plusDays(1);
        };
    }
}
