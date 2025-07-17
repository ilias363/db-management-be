package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.common.ColumnDataTypeDefinition;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnReference;
import ma.ilias.dbmanagementbe.validation.ExistingColumn;
import ma.ilias.dbmanagementbe.validation.ValidDataTypeChange;
import ma.ilias.dbmanagementbe.validation.ValidDataTypeDefinition;

@Data
@ExistingColumn
@ValidDataTypeChange
@ValidDataTypeDefinition
public class UpdateColumnDataTypeDto implements IColumnReference, ColumnDataTypeDefinition {
    @NotBlank(message = "Schema name is required")
    private String schemaName;

    @NotBlank(message = "Table name is required")
    private String tableName;

    @NotBlank(message = "Column name is required")
    private String columnName;

    @NotBlank(message = "Data type is required")
    private String dataType;

    private Integer characterMaxLength;
    private Integer numericPrecision;
    private Integer numericScale;
}
