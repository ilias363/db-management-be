package ma.ilias.dbmanagementbe.analytics.service;

import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.analytics.dto.DashboardStatsDto;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dao.repositories.AuditLogRepository;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

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
}
