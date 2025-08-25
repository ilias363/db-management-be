package ma.ilias.dbmanagementbe.analytics.dto;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UserDashboardStatsDto {
    private long totalActions;
    private long totalSuccessfulActions;
    private long totalFailedActions;
    private double successRate;
    private long last7DaysActions;
    private long last24HoursActions;
    private String mostUsedAction;
    private String mostAccessedSchema;
    private String mostAccessedTable;
    private long uniqueSchemasAccessed;
    private long uniqueTablesAccessed;
}
