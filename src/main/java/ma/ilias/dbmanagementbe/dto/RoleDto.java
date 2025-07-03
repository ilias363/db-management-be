package ma.ilias.dbmanagementbe.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.ToString;

import java.util.ArrayList;
import java.util.Collection;

public class RoleDto {
    private Long id;
    private String name; // ADMIN, VIEWER, CUSTOM_ROLE_NAME
    private String description;
    private Boolean isSystemRole; // true for ADMIN/VIEWER
    @ToString.Exclude
    @JsonIgnoreProperties("roles")
    private Collection<PermissionDto> permissions = new ArrayList<>();
}
