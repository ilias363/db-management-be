package ma.ilias.dbmanagementbe.dto.appuser;

import jakarta.validation.constraints.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.validation.ExistingRoles;
import ma.ilias.dbmanagementbe.validation.UniqueUsername;

import java.util.ArrayList;
import java.util.Collection;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@UniqueUsername
public class NewAppUserDto implements AppUserDtoBase {
    @NotBlank(message = "Username is required")
    @Size(min = 3, max = 20, message = "Username must be between 3 and 20 characters")
    private String username;

    @NotBlank(message = "Email is required")
    @Email(message = "Email should be valid")
    private String email;

    @NotNull(message = "Active status is required")
    private Boolean active;

    @NotBlank(message = "Password is required")
    @Size(min = 8, message = "Password must be at least 8 characters")
    private String password;

    @NotEmpty(message = "At least one role is required")
    @ExistingRoles
    private Collection<Long> roles = new ArrayList<>();

    @Override
    public Long getId() {
        return null;
    }
}