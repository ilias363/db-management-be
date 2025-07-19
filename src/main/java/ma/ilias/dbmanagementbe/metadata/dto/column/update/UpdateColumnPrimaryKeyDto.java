package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.EqualsAndHashCode;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseUpdateColumnDto;
import ma.ilias.dbmanagementbe.validation.annotations.ValidPrimaryKeyChange;

@Data
@EqualsAndHashCode(callSuper = true)
@ValidPrimaryKeyChange
public class UpdateColumnPrimaryKeyDto extends BaseUpdateColumnDto {
    @NotNull(message = "Primary key value is required")
    private Boolean isPrimaryKey;
}
