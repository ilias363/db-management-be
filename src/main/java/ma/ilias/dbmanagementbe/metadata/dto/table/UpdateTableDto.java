package ma.ilias.dbmanagementbe.metadata.dto.table;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.ITableReference;
import ma.ilias.dbmanagementbe.validation.ExistingTable;
import ma.ilias.dbmanagementbe.validation.UniqueTableName;

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
    private String updatedTableName;
}
