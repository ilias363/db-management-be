package ma.ilias.dbmanagementbe.dto.permission;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.dto.role.RoleDto;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class PermissionDto {
    private Long id;

    private String schemaName; // null for all schemas

    private String tableName; // null for all tables in schema

    private String permissionType;

    @ToString.Exclude
    @JsonIgnoreProperties("permissions")
    private RoleDto role;
}
