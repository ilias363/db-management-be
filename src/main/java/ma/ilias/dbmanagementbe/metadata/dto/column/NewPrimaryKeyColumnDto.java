package ma.ilias.dbmanagementbe.metadata.dto.column;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
public class NewPrimaryKeyColumnDto extends BaseNewColumnDto {

    public NewPrimaryKeyColumnDto() {
        super();
        this.setIsNullable(false);
        this.setIsUnique(true);
    }

    public NewPrimaryKeyColumnDto(String columnName, String dataType, Integer characterMaxLength,
                                  Integer numericPrecision, Integer numericScale,
                                  String columnDefault, Boolean autoIncrement) {
        super(columnName, dataType, characterMaxLength, numericPrecision, numericScale,
                false, true, columnDefault, autoIncrement);
    }

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
