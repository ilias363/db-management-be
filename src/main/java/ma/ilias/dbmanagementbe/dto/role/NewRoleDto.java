package ma.ilias.dbmanagementbe.dto.role;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.validation.ExistingPermissions;
import ma.ilias.dbmanagementbe.validation.UniqueRoleName;

import java.util.HashSet;
import java.util.Set;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@UniqueRoleName
public class NewRoleDto implements RoleDtoBase {
    @NotBlank(message = "Role name is required")
    private String name;

    private String description;

    @NotEmpty(message = "At least one permission is required")
    @ExistingPermissions
    private Set<Long> permissions = new HashSet<>();

    @Override
    public Long getId() {
        return null;
    }
}