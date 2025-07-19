package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.EqualsAndHashCode;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseUpdateColumnDto;
import ma.ilias.dbmanagementbe.validation.ValidUniqueChange;

@Data
@EqualsAndHashCode(callSuper = true)
@ValidUniqueChange
public class UpdateColumnUniqueDto extends BaseUpdateColumnDto {
    @NotNull(message = "Unique value is required")
    private Boolean isUnique;
}
