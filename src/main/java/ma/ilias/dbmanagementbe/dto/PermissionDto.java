package ma.ilias.dbmanagementbe.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import lombok.*;

import java.util.ArrayList;
import java.util.Collection;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class PermissionDto {
    @NotNull(message = "Id is required")
    private Long id;

    private String schemaName; // null for all schemas
    private String tableName; // null for all tables in schema

    @Pattern(
            regexp = "READ|WRITE|DELETE|CREATE",
            message = "Permission status is not valid"
    )
    private String permissionType;
}
