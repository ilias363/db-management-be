package ma.ilias.dbmanagementbe.metadata.dto.table;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.common.ITableReference;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingTable;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueTableName;

@Data
@UniqueTableName
@ExistingTable
public class UpdateTableDto implements ITableReference {
    @NotBlank(message = "Schema name cannot be blank")
    // schema existence checked in @UniqueTableName
    private String schemaName;

    @NotBlank(message = "Table name cannot be blank")
    private String tableName;

    @NotBlank(message = "Updated table name cannot be blank")
    @Pattern(
            regexp = "^[a-zA-Z][a-zA-Z0-9_]*$",
            message = "Table name must start with a letter and contain only alphanumeric characters and underscores"
    )
    private String updatedTableName;
}
