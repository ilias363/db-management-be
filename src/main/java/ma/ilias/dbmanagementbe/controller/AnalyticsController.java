package ma.ilias.dbmanagementbe.controller;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.analytics.dto.DashboardStatsDto;
import ma.ilias.dbmanagementbe.analytics.service.AnalyticsService;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

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
}
