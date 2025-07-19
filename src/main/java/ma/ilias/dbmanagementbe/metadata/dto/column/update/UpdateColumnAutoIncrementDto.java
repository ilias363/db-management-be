package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.EqualsAndHashCode;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseUpdateColumnDto;
import ma.ilias.dbmanagementbe.validation.annotations.ValidAutoIncrement;

@Data
@EqualsAndHashCode(callSuper = true)
@ValidAutoIncrement
public class UpdateColumnAutoIncrementDto extends BaseUpdateColumnDto {
    @NotNull(message = "Auto increment value is required")
    private Boolean autoIncrement;
}
