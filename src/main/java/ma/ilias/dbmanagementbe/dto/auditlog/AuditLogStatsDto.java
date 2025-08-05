package ma.ilias.dbmanagementbe.dto.auditlog;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ActionType;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class AuditLogStatsDto {
    private Long totalAudits;
    private Long totalSuccessful;
    private Long totalFailed;
    private Double failedPercentage;
    private Long last24hActivityCount;
    private ActionType mostCommonAction;
    private Double averageActionsPerDay;
}
