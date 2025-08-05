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
public class AuditStatsDto {
    private Long totalAudits;
    private Long totalFailed;
    private Double failedPercentage;
    private Long last24hActivityCount;
    private Long totalSuccessful;
    private ActionType mostCommonAction;
    private Double averageActionsPerDay;
}
