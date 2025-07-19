package ma.ilias.dbmanagementbe.dto.appuser;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.*;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingRoles;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueUsername;

import java.util.ArrayList;
import java.util.Collection;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
@UniqueUsername
public class UpdateAppUserDto implements AppUserDtoBase {
    @NotNull(message = "Id is required")
    private Long id;

    @NotBlank(message = "Username is required")
    @Size(min = 3, max = 20, message = "Username must be between 3 and 20 characters")
    private String username;

    @NotNull(message = "Active status is required")
    private Boolean active;

    @NotEmpty(message = "At least one role is required")
    @ExistingRoles
    @ToString.Exclude
    @Builder.Default
    private Collection<Long> roles = new ArrayList<>();
}
