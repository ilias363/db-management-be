package ma.ilias.dbmanagementbe.dto.role;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingPermissions;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueRoleName;

import java.util.HashSet;
import java.util.Set;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@UniqueRoleName
public class UpdateRoleDto implements RoleDtoBase {
    @NotNull(message = "Id is required")
    private Long id;

    @NotBlank(message = "Role name is required")
    private String name; // ADMIN, VIEWER, CUSTOM_ROLE_NAME

    private String description;

    @NotEmpty(message = "At least one permission is required")
    @ExistingPermissions
    private Set<Long> permissions = new HashSet<>();
}
