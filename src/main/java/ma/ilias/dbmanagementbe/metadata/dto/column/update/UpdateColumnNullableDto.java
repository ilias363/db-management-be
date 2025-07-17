package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnReference;
import ma.ilias.dbmanagementbe.validation.ExistingColumn;
import ma.ilias.dbmanagementbe.validation.ValidNullableChange;

@Data
@ExistingColumn
@ValidNullableChange
public class UpdateColumnNullableDto implements IColumnReference {
    @NotBlank(message = "Schema name is required")
    private String schemaName;

    @NotBlank(message = "Table name is required")
    private String tableName;

    @NotBlank(message = "Column name is required")
    private String columnName;

    @NotNull(message = "Nullable value is required")
    private Boolean isNullable;
}
