package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.EqualsAndHashCode;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseUpdateColumnDto;
import ma.ilias.dbmanagementbe.validation.ValidColumnDefaultUpdate;

@Data
@EqualsAndHashCode(callSuper = true)
@ValidColumnDefaultUpdate
public class UpdateColumnDefaultDto extends BaseUpdateColumnDto {
    @NotNull(message = "Column default is required (set to 'null' to remove default)")
    private String columnDefault;
}
