package ma.ilias.dbmanagementbe.analytics.service;

import ma.ilias.dbmanagementbe.analytics.dto.*;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogDto;
import ma.ilias.dbmanagementbe.enums.DatabaseType;

import java.time.LocalDateTime;
import java.util.List;

public interface AnalyticsService {
    List<DatabaseUsageDto> getDatabaseUsage(boolean includeSystem);

    DatabaseType getDatabaseType();

    DatabaseStatsDto getDatabaseStats(boolean includeSystem);

    DashboardStatsDto getDashboardStats(boolean includeSystem);

    List<UserActivityDto> getUserActivity(LocalDateTime startDate, LocalDateTime endDate, String period);

    List<TopUsersByActivityDto> getTopUsersByActivity(LocalDateTime startDate, LocalDateTime endDate, Integer limit);

    List<RoleDistributionDto> getRoleDistribution();

    List<AuditActivityDto> getAuditActivity(LocalDateTime startDate, LocalDateTime endDate, String period);

    List<AuditHeatmapDto> getAuditHeatmap(LocalDateTime startDate, LocalDateTime endDate);

    List<AuditHeatmapDto> getAuditHeatmapAllTime();

    UserDashboardStatsDto getUserDashboardStats();

    List<UserActionBreakdownDto> getUserActionBreakdown();

    List<AuditLogDto> getUserRecentActivity(Integer limit);
}
