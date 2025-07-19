package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import lombok.EqualsAndHashCode;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseUpdateColumnDto;
import ma.ilias.dbmanagementbe.validation.UniqueColumnName;

@Data
@EqualsAndHashCode(callSuper = true)
@UniqueColumnName
public class RenameColumnDto extends BaseUpdateColumnDto {
    @NotBlank(message = "New column name is required")
    @Pattern(regexp = "^[a-zA-Z][a-zA-Z0-9_]*$",
            message = "New column name must start with a letter and contain only letters, numbers, and underscores")
    private String newColumnName;
}
