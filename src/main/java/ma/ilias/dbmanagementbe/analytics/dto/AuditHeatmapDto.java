package ma.ilias.dbmanagementbe.analytics.dto;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class AuditHeatmapDto {
    private Integer dayOfWeek; // 1 = Monday, 2 = Tuesday, ... 7 = Sunday
    private Integer hourOfDay; // 0-23
    private Long activityCount;
}
