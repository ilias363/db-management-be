package ma.ilias.dbmanagementbe.analytics.service;

import ma.ilias.dbmanagementbe.analytics.dto.DashboardStatsDto;
import ma.ilias.dbmanagementbe.analytics.dto.UserActivityDto;

import java.time.LocalDateTime;
import java.util.List;

public interface AnalyticsService {
    DashboardStatsDto getDashboardStats(boolean includeSystem);

    List<UserActivityDto> getUserActivity(LocalDateTime startDate, LocalDateTime endDate, String period);
}
