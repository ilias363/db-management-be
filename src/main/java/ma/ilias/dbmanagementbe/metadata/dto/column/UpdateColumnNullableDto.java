package ma.ilias.dbmanagementbe.metadata.dto.column;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import ma.ilias.dbmanagementbe.validation.NotNullableWithDefault;

@Data
@NotNullableWithDefault
public class UpdateColumnNullableDto {
    @NotBlank(message = "Schema name cannot be blank")
    private String schemaName;

    @NotBlank(message = "Table name cannot be blank")
    private String tableName;

    @NotBlank(message = "Column name cannot be blank")
    private String columnName;

    @NotNull(message = "The isNullable field cannot be null.")
    private Boolean isNullable;

    private String defaultValue;
}