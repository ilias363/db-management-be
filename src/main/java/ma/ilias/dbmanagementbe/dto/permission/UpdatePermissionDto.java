package ma.ilias.dbmanagementbe.dto.permission;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Pattern;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.validation.ValidPermissionType;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class UpdatePermissionDto {
    @NotEmpty(message = "Schema name is required")
    private String schemaName; // '*' for all schemas

    @NotEmpty(message = "Table name is required")
    private String tableName; // '*' for all tables in schema

    @ValidPermissionType
    @NotEmpty(message = "Permission type is required")
    private String permissionType;
}
