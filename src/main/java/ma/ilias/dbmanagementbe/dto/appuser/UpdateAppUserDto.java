package ma.ilias.dbmanagementbe.dto.appuser;

import jakarta.validation.constraints.*;
import lombok.*;
import ma.ilias.dbmanagementbe.validation.ExistingRoles;
import ma.ilias.dbmanagementbe.validation.UniqueUsername;

import java.util.ArrayList;
import java.util.Collection;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class UpdateAppUserDto {
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
    private Collection<Long> roles = new ArrayList<>();
}
