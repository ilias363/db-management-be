package ma.ilias.dbmanagementbe.metadata.dto.column;

import jakarta.validation.constraints.NotNull;
import lombok.Data;
import ma.ilias.dbmanagementbe.validation.NotNullableWithDefault;

@Data
@NotNullableWithDefault
public class UpdateColumnNullableDto {
    @NotNull(message = "The isNullable field cannot be null.")
    private Boolean isNullable;

    private String defaultValue;
}