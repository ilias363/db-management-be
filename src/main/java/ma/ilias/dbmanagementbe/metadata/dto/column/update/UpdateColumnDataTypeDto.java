package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import lombok.EqualsAndHashCode;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseUpdateColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.common.ColumnDataTypeDefinition;
import ma.ilias.dbmanagementbe.validation.annotations.ValidDataTypeChange;
import ma.ilias.dbmanagementbe.validation.annotations.ValidDataTypeDefinition;

@Data
@EqualsAndHashCode(callSuper = true)
@ValidDataTypeChange
@ValidDataTypeDefinition
public class UpdateColumnDataTypeDto extends BaseUpdateColumnDto implements ColumnDataTypeDefinition {
    @NotBlank(message = "Data type is required")
    private String dataType;

    private Integer characterMaxLength;
    private Integer numericPrecision;
    private Integer numericScale;
}
