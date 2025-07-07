package ma.ilias.dbmanagementbe.dto.permission;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.dto.role.RoleDto;
import ma.ilias.dbmanagementbe.validation.ValidPermissionType;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class PermissionDto {
    @NotNull(message = "Id is required")
    private Long id;

    @NotEmpty(message = "Schema name is required")
    private String schemaName; // '*' for all schemas

    @NotEmpty(message = "Table name is required")
    private String tableName; // '*' for all tables in schema

    @ValidPermissionType
    @NotEmpty(message = "Permission type is required")
    private String permissionType;

    @JsonIgnoreProperties("permissions")
    private RoleDto role;
}
