package ma.ilias.dbmanagementbe.metadata.dto.column.primarykey;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;

@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class NewPrimaryKeyColumnDto extends BaseNewColumnDto {
    @Override
    public void setIsNullable(Boolean isNullable) {
        // Primary keys cannot be nullable
        super.setIsNullable(false);
    }

    @Override
    public void setIsUnique(Boolean isUnique) {
        // Primary keys are always unique
        super.setIsUnique(true);
    }
}
