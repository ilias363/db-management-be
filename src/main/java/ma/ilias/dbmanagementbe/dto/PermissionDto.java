package ma.ilias.dbmanagementbe.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.*;

import java.util.ArrayList;
import java.util.Collection;

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
    @JsonIgnore
    private Collection<RoleDto> roles = new ArrayList<>();
}
