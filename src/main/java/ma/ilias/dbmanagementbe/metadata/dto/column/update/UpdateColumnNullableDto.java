package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.EqualsAndHashCode;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseUpdateColumnDto;
import ma.ilias.dbmanagementbe.validation.annotations.ValidNullableChange;

@Data
@EqualsAndHashCode(callSuper = true)
@ValidNullableChange
public class UpdateColumnNullableDto extends BaseUpdateColumnDto {
    @NotNull(message = "Nullable value is required")
    private Boolean isNullable;
}
