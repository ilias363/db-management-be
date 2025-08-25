package ma.ilias.dbmanagementbe.analytics.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ma.ilias.dbmanagementbe.analytics.dto.*;
import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import ma.ilias.dbmanagementbe.dao.entities.AuditLog;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dao.repositories.AuditLogRepository;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogDto;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.enums.DatabaseType;
import ma.ilias.dbmanagementbe.mapper.AuditLogMapper;
import ma.ilias.dbmanagementbe.util.AuthorizationUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class MySqlAnalyticsManager implements AnalyticsService {

    private final AppUserRepository appUserRepository;
    private final AuditLogRepository auditLogRepository;
    private final RoleRepository roleRepository;
    private final JdbcTemplate jdbcTemplate;
    private final DatabaseType databaseType;
    private final AuditLogMapper auditLogMapper;

    @Override
    public List<DatabaseUsageDto> getDatabaseUsage(boolean includeSystem) {
        String sql = "SELECT " +
                "table_schema as schema_name, " +
                "COUNT(*) as table_count, " +
                "SUM(table_rows) as record_count, " +
                "SUM(data_length) as data_size_bytes, " +
                "SUM(index_length) as index_size_bytes, " +
                "MAX(update_time) as last_accessed " +
                "FROM information_schema.tables WHERE table_type = 'BASE TABLE' " +
                (includeSystem ? "" : "AND table_schema NOT IN ('information_schema', 'mysql', 'performance_schema', 'sys') ") +
                "GROUP BY table_schema " +
                "ORDER BY data_size_bytes + index_size_bytes DESC";

        return jdbcTemplate.query(sql,
                (rs, rowNum) -> DatabaseUsageDto.builder()
                        .schemaName(rs.getString("schema_name"))
                        .tableCount((long) rs.getInt("table_count"))
                        .recordCount(rs.getLong("record_count"))
                        .size((long) rs.getDouble("data_size_bytes") + (long) rs.getDouble("index_size_bytes"))
                        .lastModified(rs.getTimestamp("last_accessed") != null ?
                                rs.getTimestamp("last_accessed").toString() :
                                "unknown")
                        .build()
        );
    }

    @Override
    public DatabaseType getDatabaseType() {
        return databaseType;
    }

    @Override
    public DatabaseStatsDto getDatabaseStats(boolean includeSystem) {
        return DatabaseStatsDto.builder()
                .totalSchemas(getDatabaseSchemaCount(includeSystem))
                .totalTables(getDatabaseTableCount(includeSystem))
                .totalViews(getDatabaseViewCount(includeSystem))
                .totalRecords(getDatabaseRecordCount(includeSystem))
                .build();
    }

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
    public List<TopUsersByActivityDto> getTopUsersByActivity(LocalDateTime startDate, LocalDateTime endDate, Integer limit) {
        Pageable pageable = PageRequest.of(0, limit != null ? limit : 10);
        List<Object[]> results = auditLogRepository.findTopUsersByActivity(startDate, endDate, pageable);

        return results.stream()
                .map(row -> TopUsersByActivityDto.builder()
                        .username((String) row[0])
                        .actionCount(((Number) row[1]).longValue())
                        .lastActivity(row[2] != null ? row[2].toString() : "unknown")
                        .build())
                .toList();
    }

    @Override
    public List<RoleDistributionDto> getRoleDistribution() {
        List<Object[]> results = roleRepository.findRoleDistribution();

        return results.stream()
                .map(row -> RoleDistributionDto.builder()
                        .roleName((String) row[0])
                        .userCount(((Number) row[1]).longValue())
                        .build())
                .toList();
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

    @Override
    public List<AuditHeatmapDto> getAuditHeatmap(LocalDateTime startDate, LocalDateTime endDate) {
        List<Object[]> results = auditLogRepository.findAuditHeatmapData(startDate, endDate);

        return processHeatmapResults(results);
    }

    @Override
    public List<AuditHeatmapDto> getAuditHeatmapAllTime() {
        List<Object[]> results = auditLogRepository.findAuditHeatmapDataAllTime();

        return processHeatmapResults(results);
    }

    private List<AuditHeatmapDto> processHeatmapResults(List<Object[]> results) {
        // Group by day of week and hour, counting occurrences
        Map<String, Long> groupedData = results.stream()
                .collect(Collectors.groupingBy(
                        row -> {
                            LocalDateTime timestamp = (LocalDateTime) row[0];
                            int hour = ((Number) row[1]).intValue();
                            Integer dayOfWeek = timestamp.getDayOfWeek().getValue(); // Monday=1, Sunday=7
                            return dayOfWeek + "-" + hour;
                        },
                        Collectors.counting()
                ));

        return groupedData.entrySet().stream()
                .map(entry -> {
                    String[] parts = entry.getKey().split("-");
                    return AuditHeatmapDto.builder()
                            .dayOfWeek(Integer.parseInt(parts[0]))
                            .hourOfDay(Integer.parseInt(parts[1]))
                            .activityCount(entry.getValue())
                            .build();
                })
                .toList();
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

    @Override
    public UserDashboardStatsDto getUserDashboardStats() {
        AppUser currentUser = AuthorizationUtils.getCurrentUser();
        if (currentUser == null) {
            return UserDashboardStatsDto.builder().build();
        }
        long userId = currentUser.getId();

        LocalDateTime now = LocalDateTime.now();
        LocalDateTime sevenDaysAgo = now.minusDays(7);
        LocalDateTime oneDayAgo = now.minusDays(1);

        long totalActions = auditLogRepository.countByUser_Id(userId);
        long successfulActions = auditLogRepository.countByUser_IdAndSuccessful(userId, true);
        long failedActions = totalActions - successfulActions;
        double successRate = totalActions > 0 ? (double) successfulActions / totalActions * 100 : 0.0;

        long last7DaysActions = auditLogRepository.countByUserIdAndAuditTimestampAfter(userId, sevenDaysAgo);
        long last24HoursActions = auditLogRepository.countByUserIdAndAuditTimestampAfter(userId, oneDayAgo);

        String mostUsedAction = auditLogRepository.findTopActionTypeByUserId(userId).orElse("N/A");

        String mostAccessedSchema = auditLogRepository.findTopSchemaByUserId(userId).orElse("N/A");
        String mostAccessedTable = auditLogRepository.findTopTableByUserId(userId).orElse("N/A");

        long uniqueSchemas = auditLogRepository.countUniqueSchemasByUserId(userId);
        long uniqueTables = auditLogRepository.countUniqueTablesByUserId(userId);

        return UserDashboardStatsDto.builder()
                .totalActions(totalActions)
                .totalSuccessfulActions(successfulActions)
                .totalFailedActions(failedActions)
                .successRate(successRate)
                .last7DaysActions(last7DaysActions)
                .last24HoursActions(last24HoursActions)
                .mostUsedAction(mostUsedAction)
                .mostAccessedSchema(mostAccessedSchema)
                .mostAccessedTable(mostAccessedTable)
                .uniqueSchemasAccessed(uniqueSchemas)
                .uniqueTablesAccessed(uniqueTables)
                .build();
    }

    @Override
    public List<UserActionBreakdownDto> getUserActionBreakdown() {
        AppUser currentUser = AuthorizationUtils.getCurrentUser();
        if (currentUser == null) {
            return List.of(UserActionBreakdownDto.builder().build());
        }
        long userId = currentUser.getId();

        List<Object[]> results = auditLogRepository.findUserActionBreakdown(userId);
        long totalActions = results.stream().mapToLong(row -> (Long) row[1]).sum();

        return results.stream()
                .map(row -> {
                    long count = (Long) row[1];
                    double percentage = totalActions > 0 ? (double) count / totalActions * 100 : 0.0;
                    return UserActionBreakdownDto.builder()
                            .actionType((ActionType) row[0])
                            .actionCount(count)
                            .percentage(percentage)
                            .build();
                })
                .toList();
    }

    @Override
    public List<AuditLogDto> getUserRecentActivity(Integer limit) {
        AppUser currentUser = AuthorizationUtils.getCurrentUser();
        if (currentUser == null) {
            return List.of(AuditLogDto.builder().build());
        }
        long userId = currentUser.getId();

        Pageable pageable = PageRequest.of(0, limit != null ? limit : 10);
        Page<AuditLog> auditLogs = auditLogRepository.findByUser_Id(userId, pageable);

        return auditLogs.getContent().stream()
                .peek(audit -> audit.setUser(null))
                .map(auditLogMapper::toDto)
                .toList();
    }

    @Override
    public List<UserDatabaseAccessDto> getUserDatabaseAccess() {
        AppUser currentUser = AuthorizationUtils.getCurrentUser();
        if (currentUser == null) {
            return List.of(UserDatabaseAccessDto.builder().build());
        }
        long userId = currentUser.getId();

        List<Object[]> results = auditLogRepository.findUserDatabaseAccess(userId);

        return results.stream()
                .map(row -> UserDatabaseAccessDto.builder()
                        .schemaName((String) row[0])
                        .accessCount((Long) row[1])
                        .lastAccessed((LocalDateTime) row[2])
                        .build())
                .toList();
    }

    @Override
    public List<AuditHeatmapDto> getUserAuditHeatmapAllTime() {
        AppUser currentUser = AuthorizationUtils.getCurrentUser();
        if (currentUser == null) {
            return List.of(AuditHeatmapDto.builder().build());
        }
        long userId = currentUser.getId();

        List<Object[]> rawData = auditLogRepository.findUserAuditHeatmapData(userId);
        return processHeatmapResults(rawData);
    }
}
