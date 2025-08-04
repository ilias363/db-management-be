package ma.ilias.dbmanagementbe.dto.role;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class RoleStatsDto {
    Long totalRoles;
    Long systemRoles;
    Long customRoles;
    Long roleAssignations;
}
