package ma.ilias.dbmanagementbe.analytics.service;

import ma.ilias.dbmanagementbe.analytics.dto.*;

import java.time.LocalDateTime;
import java.util.List;

public interface AnalyticsService {
    DashboardStatsDto getDashboardStats(boolean includeSystem);

    List<UserActivityDto> getUserActivity(LocalDateTime startDate, LocalDateTime endDate, String period);

    List<DatabaseUsageDto> getDatabaseUsage(boolean includeSystem);

    List<AuditActivityDto> getAuditActivity(LocalDateTime startDate, LocalDateTime endDate, String period);

    List<TopUsersByActivityDto> getTopUsersByActivity(LocalDateTime startDate, LocalDateTime endDate, Integer limit);
}
