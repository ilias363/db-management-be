package ma.ilias.dbmanagementbe.analytics.service;

import ma.ilias.dbmanagementbe.analytics.dto.DashboardStatsDto;

public interface AnalyticsService {
    DashboardStatsDto getDashboardStats(boolean includeSystem);
}
