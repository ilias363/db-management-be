package ma.ilias.dbmanagementbe.analytics.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AuditActivityDto {
    private String date;
    private Long totalActions;
    private Long successfulActions;
    private Long failedActions;
    private Long activeUsers;
}
