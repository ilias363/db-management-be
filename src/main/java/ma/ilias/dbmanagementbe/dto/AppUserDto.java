package ma.ilias.dbmanagementbe.dto;

import lombok.*;

import java.util.ArrayList;
import java.util.Collection;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class AppUserDto {
    private Long id;
    private String username;
    private String email;
    private Boolean active;
    @ToString.Exclude
    private Collection<RoleDto> roles = new ArrayList<>();
}
