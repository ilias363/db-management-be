package ma.ilias.dbmanagementbe.metadata.dto.column.standard;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;

@Data
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
public class NewStandardColumnDto extends BaseNewColumnDto {

    public NewStandardColumnDto() {
        super();
    }

    public NewStandardColumnDto(String columnName, String dataType, Integer characterMaxLength,
                                Integer numericPrecision, Integer numericScale, Boolean isNullable,
                                Boolean isUnique, String columnDefault, Boolean autoIncrement) {
        super(columnName, dataType, characterMaxLength, numericPrecision, numericScale,
                isNullable, isUnique, columnDefault, autoIncrement);
    }
}
