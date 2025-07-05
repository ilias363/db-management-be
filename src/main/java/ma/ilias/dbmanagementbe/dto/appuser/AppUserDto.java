package ma.ilias.dbmanagementbe.dto.appuser;

import jakarta.validation.constraints.*;
import lombok.*;
import ma.ilias.dbmanagementbe.dto.role.RoleDto;
import ma.ilias.dbmanagementbe.validation.ExistingRoles;
import ma.ilias.dbmanagementbe.validation.UniqueUsername;

import java.util.ArrayList;
import java.util.Collection;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class AppUserDto implements AppUserDtoBase {
    @NotNull(message = "Id is required")
    private Long id;

    @NotBlank(message = "Username is required")
    @Size(min = 3, max = 20, message = "Username must be between 3 and 20 characters")
    @UniqueUsername
    private String username;

    @NotBlank(message = "Email is required")
    @Email(message = "Email should be valid")
    private String email;

    @NotNull(message = "Active status is required")
    private Boolean active;

    @NotEmpty(message = "At least one role is required")
    @ExistingRoles
    @ToString.Exclude
    private Collection<RoleDto> roles = new ArrayList<>();
}
