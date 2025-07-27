package ma.ilias.dbmanagementbe.dto.permission;

import jakarta.validation.constraints.NotEmpty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.validation.annotations.ValidPermissionFields;
import ma.ilias.dbmanagementbe.validation.annotations.ValidPermissionType;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@ValidPermissionFields
public class PermissionDetailDto {
    private String schemaName;

    private String tableName;
    private String viewName;

    @ValidPermissionType
    @NotEmpty(message = "Permission type is required")
    private String permissionType;
}
