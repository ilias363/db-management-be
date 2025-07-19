package ma.ilias.dbmanagementbe.dto.role;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.*;
import ma.ilias.dbmanagementbe.dto.permission.PermissionDto;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingPermissions;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueRoleName;

import java.util.HashSet;
import java.util.Set;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@UniqueRoleName
public class RoleDto implements RoleDtoBase {
    @NotNull(message = "Id is required")
    private Long id;

    @NotBlank(message = "Role name is required")
    private String name; // ADMIN, VIEWER, CUSTOM_ROLE_NAME

    private String description;

    @NotEmpty(message = "At least one permission is required")
    @ExistingPermissions
    @ToString.Exclude
    @JsonIgnoreProperties("role")
    @Builder.Default
    private Set<PermissionDto> permissions = new HashSet<>();
}
