package ma.ilias.dbmanagementbe.controller;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.analytics.dto.DashboardStatsDto;
import ma.ilias.dbmanagementbe.analytics.dto.UserActivityDto;
import ma.ilias.dbmanagementbe.analytics.service.AnalyticsService;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
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
