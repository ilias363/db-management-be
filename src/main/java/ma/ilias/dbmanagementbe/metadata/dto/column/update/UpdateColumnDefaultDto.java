package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnReference;
import ma.ilias.dbmanagementbe.validation.ExistingColumn;
import ma.ilias.dbmanagementbe.validation.ValidColumnDefaultUpdate;

@Data
@ExistingColumn
@ValidColumnDefaultUpdate
public class UpdateColumnDefaultDto implements IColumnReference {
    @NotBlank(message = "Schema name is required")
    private String schemaName;

    @NotBlank(message = "Table name is required")
    private String tableName;

    @NotBlank(message = "Column name is required")
    private String columnName;

    // Can be null to remove default value
    private String columnDefault;
}
