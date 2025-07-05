package ma.ilias.dbmanagementbe.dto.role;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.*;
import ma.ilias.dbmanagementbe.validation.ExistingPermissions;
import ma.ilias.dbmanagementbe.validation.UniqueRoleName;

import java.util.ArrayList;
import java.util.Collection;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class UpdateRoleDto {
    @NotNull(message = "Id is required")
    private Long id;

    @NotBlank(message = "Role name is required")
    @UniqueRoleName
    private String name; // ADMIN, VIEWER, CUSTOM_ROLE_NAME

    private String description;

    @NotEmpty(message = "At least one permission is required")
    @ExistingPermissions
    private Collection<Long> permissions = new ArrayList<>();
}
