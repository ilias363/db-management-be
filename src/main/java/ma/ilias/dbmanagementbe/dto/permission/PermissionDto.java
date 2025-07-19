package ma.ilias.dbmanagementbe.dto.permission;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.dto.role.RoleDto;
import ma.ilias.dbmanagementbe.validation.annotations.ValidPermissionType;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class PermissionDto {
    @NotNull(message = "Id is required")
    private Long id;

    @NotEmpty(message = "Schema name is required")
    private String schemaName; // null for all schemas

    @NotEmpty(message = "Table name is required")
    private String tableName; // null for all tables in schema

    @ValidPermissionType
    @NotEmpty(message = "Permission type is required")
    private String permissionType;

    @NotNull(message = "Role is required")
    @JsonIgnoreProperties("permissions")
    private RoleDto role;
}
