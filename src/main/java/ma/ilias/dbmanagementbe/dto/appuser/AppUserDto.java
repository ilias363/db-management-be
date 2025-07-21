package ma.ilias.dbmanagementbe.dto.appuser;

import lombok.*;
import ma.ilias.dbmanagementbe.dto.role.RoleDto;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingRoles;

import java.util.ArrayList;
import java.util.Collection;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class AppUserDto implements AppUserDtoBase {
    private Long id;
    private String username;
    private Boolean active;

    @ExistingRoles
    @ToString.Exclude
    @Builder.Default
    private Collection<RoleDto> roles = new ArrayList<>();
}
