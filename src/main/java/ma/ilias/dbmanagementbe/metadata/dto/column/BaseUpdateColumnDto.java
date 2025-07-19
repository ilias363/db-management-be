package ma.ilias.dbmanagementbe.metadata.dto.column;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnReference;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingColumn;
import ma.ilias.dbmanagementbe.validation.annotations.NotSystemSchema;

@Data
@ExistingColumn
public abstract class BaseUpdateColumnDto implements IColumnReference {
    @NotBlank(message = "Schema name is required")
    @NotSystemSchema
    private String schemaName;

    @NotBlank(message = "Table name is required")
    private String tableName;

    @NotBlank(message = "Column name is required")
    private String columnName;
}
