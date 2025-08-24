package ma.ilias.dbmanagementbe.analytics.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RoleDistributionDto {
    private String roleName;
    private Long userCount;
}
