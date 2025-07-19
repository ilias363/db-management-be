package ma.ilias.dbmanagementbe.dto.appuser;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingRoles;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueUsername;

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

    @NotNull(message = "Active status is required")
    private Boolean active;

    @NotBlank(message = "Password is required")
    @Size(min = 8, message = "Password must be at least 8 characters")
    private String password;

    @NotEmpty(message = "At least one role is required")
    @ExistingRoles
    @Builder.Default
    private Collection<Long> roles = new ArrayList<>();

    @Override
    public Long getId() {
        return null;
    }
}