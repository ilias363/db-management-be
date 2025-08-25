package ma.ilias.dbmanagementbe.controller;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.analytics.dto.*;
import ma.ilias.dbmanagementbe.analytics.service.AnalyticsService;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.enums.DatabaseType;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDateTime;
import java.util.List;

@RestController
@RequestMapping("/api/analytics")
@AllArgsConstructor
public class AnalyticsController {

    private final AnalyticsService analyticsService;

    @GetMapping("/database/usage")
    public ResponseEntity<ApiResponse<List<DatabaseUsageDto>>> getDatabaseUsage(
            @RequestParam(defaultValue = "false") boolean includeSystem
    ) {
        List<DatabaseUsageDto> usage = analyticsService.getDatabaseUsage(includeSystem);
        return ResponseEntity.ok(ApiResponse.<List<DatabaseUsageDto>>builder()
                .message("Database usage fetched successfully")
                .success(true)
                .data(usage)
                .build());
    }

    @GetMapping("/database/type")
    public ResponseEntity<ApiResponse<DatabaseTypeDto>> getDatabaseType() {
        DatabaseType type = analyticsService.getDatabaseType();
        DatabaseTypeDto dto = new DatabaseTypeDto(type.name(), type.getDisplayName());
        return ResponseEntity.ok(ApiResponse.<DatabaseTypeDto>builder()
                .message("Database type fetched successfully")
                .success(true)
                .data(dto)
                .build());
    }

    @GetMapping("/database/stats")
    public ResponseEntity<ApiResponse<DatabaseStatsDto>> getDatabaseStats(
            @RequestParam(defaultValue = "false") boolean includeSystem
    ) {
        DatabaseStatsDto stats = analyticsService.getDatabaseStats(includeSystem);
        return ResponseEntity.ok(ApiResponse.<DatabaseStatsDto>builder()
                .message("Database stats fetched successfully")
                .success(true)
                .data(stats)
                .build());
    }

    @GetMapping("/dashboard/stats")
    public ResponseEntity<ApiResponse<DashboardStatsDto>> getDashboardStats(
            @RequestParam(defaultValue = "false") boolean includeSystem
    ) {
        DashboardStatsDto stats = analyticsService.getDashboardStats(includeSystem);
        return ResponseEntity.ok(ApiResponse.<DashboardStatsDto>builder()
                .message("Dashboard stats fetched successfully")
                .success(true)
                .data(stats)
                .build());
    }

    @GetMapping("/users/activity")
    public ResponseEntity<ApiResponse<List<UserActivityDto>>> getUserActivity(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime endDate,
            @RequestParam(defaultValue = "day") String period) {

        if (startDate == null) startDate = getStartDateByPeriod(period);
        if (endDate == null) endDate = LocalDateTime.now();

        List<UserActivityDto> activity = analyticsService.getUserActivity(startDate, endDate, period);
        return ResponseEntity.ok(ApiResponse.<List<UserActivityDto>>builder()
                .message("User activity fetched successfully")
                .success(true)
                .data(activity)
                .build());
    }

    @GetMapping("/users/top-by-activity")
    public ResponseEntity<ApiResponse<List<TopUsersByActivityDto>>> getTopUsersByActivity(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime endDate,
            @RequestParam(defaultValue = "week") String period,
            @RequestParam(defaultValue = "10") Integer limit) {

        if (startDate == null) startDate = getStartDateByPeriod(period);
        if (endDate == null) endDate = LocalDateTime.now();

        List<TopUsersByActivityDto> topUsers = analyticsService.getTopUsersByActivity(startDate, endDate, limit);
        return ResponseEntity.ok(ApiResponse.<List<TopUsersByActivityDto>>builder()
                .message("Top users by activity fetched successfully")
                .success(true)
                .data(topUsers)
                .build());
    }

    @GetMapping("/roles/distribution")
    public ResponseEntity<ApiResponse<List<RoleDistributionDto>>> getRoleDistribution() {
        List<RoleDistributionDto> distribution = analyticsService.getRoleDistribution();
        return ResponseEntity.ok(ApiResponse.<List<RoleDistributionDto>>builder()
                .message("Role distribution fetched successfully")
                .success(true)
                .data(distribution)
                .build());
    }

    @GetMapping("/audit/activity")
    public ResponseEntity<ApiResponse<List<AuditActivityDto>>> getAuditActivity(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime endDate,
            @RequestParam(defaultValue = "day") String period) {

        if (startDate == null) startDate = getStartDateByPeriod(period);
        if (endDate == null) endDate = LocalDateTime.now();

        List<AuditActivityDto> activity = analyticsService.getAuditActivity(startDate, endDate, period);
        return ResponseEntity.ok(ApiResponse.<List<AuditActivityDto>>builder()
                .message("Audit activity fetched successfully")
                .success(true)
                .data(activity)
                .build());
    }

    @GetMapping("/audit/heatmap")
    public ResponseEntity<ApiResponse<List<AuditHeatmapDto>>> getAuditHeatmap(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime endDate) {

        List<AuditHeatmapDto> heatmap;

        if (startDate == null && endDate == null) {
            heatmap = analyticsService.getAuditHeatmapAllTime();
        } else {
            if (startDate == null) startDate = LocalDateTime.now().minusDays(30);
            if (endDate == null) endDate = LocalDateTime.now();
            heatmap = analyticsService.getAuditHeatmap(startDate, endDate);
        }

        return ResponseEntity.ok(ApiResponse.<List<AuditHeatmapDto>>builder()
                .message("Audit heatmap fetched successfully")
                .success(true)
                .data(heatmap)
                .build());
    }

    private LocalDateTime getStartDateByPeriod(String period) {
        LocalDateTime now = LocalDateTime.now();
        return switch (period.toLowerCase()) {
            case "hour" -> now.minusHours(24);
            case "day" -> now.minusDays(7);
            case "week" -> now.minusWeeks(4);
            case "month" -> now.minusMonths(12);
            default -> now.minusDays(7);
        };
    }
}
