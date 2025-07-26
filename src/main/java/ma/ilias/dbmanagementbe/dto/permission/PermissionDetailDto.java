package ma.ilias.dbmanagementbe.dto.permission;

import jakarta.validation.constraints.NotEmpty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingSchemaAndTable;
import ma.ilias.dbmanagementbe.validation.annotations.ValidPermissionType;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@ExistingSchemaAndTable
public class PermissionDetailDto {
    private String schemaName; // null for all schemas

    private String tableName; // null for all tables in schema

    @ValidPermissionType
    @NotEmpty(message = "Permission type is required")
    private String permissionType;
}
