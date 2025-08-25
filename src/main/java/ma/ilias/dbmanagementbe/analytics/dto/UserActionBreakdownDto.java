package ma.ilias.dbmanagementbe.analytics.dto;

import lombok.Builder;
import lombok.Data;
import ma.ilias.dbmanagementbe.enums.ActionType;

@Data
@Builder
public class UserActionBreakdownDto {
    private ActionType actionType;
    private long actionCount;
    private double percentage;
}
