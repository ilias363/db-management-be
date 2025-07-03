package ma.ilias.dbmanagementbe.dto;

import jakarta.validation.constraints.Pattern;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class NewPermissionDto {
    private String schemaName; // null for all schemas
    private String tableName; // null for all tables in schema
    @Pattern(
            regexp = "READ|WRITE|DELETE|CREATE",
            message = "Permission status is not valid"
    )
    private String permissionType;
}
