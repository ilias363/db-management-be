package ma.ilias.dbmanagementbe.analytics.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DashboardStatsDto {
    private Long totalUsers;
    private Long totalActiveUsers;
    private Long totalRoles;
    private Long totalAudits;
    private Long totalSchemas;
    private Long totalTables;
    private Long totalViews;
    private Long totalRecords;
    private Long last7DaysActivity;
}
