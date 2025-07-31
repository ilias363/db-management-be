package ma.ilias.dbmanagementbe.dto.appuser;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class AppUserStatsDto {
    private Long totalUsers;
    private Long activeUsers;
    private Long inactiveUsers;
    private Long adminUsers;
    private Long newThisMonth;
    private Double activeUserPercentage;
}
