package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnReference;
import ma.ilias.dbmanagementbe.validation.ExistingColumn;
import ma.ilias.dbmanagementbe.validation.UniqueColumnName;

@Data
@ExistingColumn
@UniqueColumnName
public class RenameColumnDto implements IColumnReference {
    @NotBlank(message = "Schema name is required")
    private String schemaName;

    @NotBlank(message = "Table name is required")
    private String tableName;

    @NotBlank(message = "Column name is required")
    private String columnName;

    @NotBlank(message = "New column name is required")
    @Pattern(regexp = "^[a-zA-Z][a-zA-Z0-9_]*$",
            message = "New column name must start with a letter and contain only letters, numbers, and underscores")
    private String newColumnName;
}
