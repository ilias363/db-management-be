package ma.ilias.dbmanagementbe.dto.permission;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import lombok.*;

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

    @Pattern(
            regexp = "READ|WRITE|DELETE|CREATE",
            message = "Permission status is not valid"
    )
    private String permissionType;
}
