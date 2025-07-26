package ma.ilias.dbmanagementbe.dto.role;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.dto.permission.PermissionDetailDto;

import java.util.HashSet;
import java.util.Set;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class RoleDto implements RoleDtoBase {
    private Long id;
    private String name; // ADMIN, VIEWER, CUSTOM_ROLE_NAME
    private String description;

    @ToString.Exclude
    @JsonIgnoreProperties("role")
    @Builder.Default
    private Set<PermissionDetailDto> permissions = new HashSet<>();
}
