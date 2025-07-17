package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.NewPrimaryKeyColumnDto;

public class ValidAutoIncrementValidator implements ConstraintValidator<ValidAutoIncrement, NewPrimaryKeyColumnDto> {

    @Override
    public boolean isValid(NewPrimaryKeyColumnDto dto, ConstraintValidatorContext context) {
        if (dto == null) {
            return true;
        }

        Boolean autoIncrement = dto.getAutoIncrement();
        String dataType = dto.getDataType();

        if (autoIncrement == null || !autoIncrement || dataType == null) {
            return true;
        }

        return dataType.equalsIgnoreCase("INT") ||
                dataType.equalsIgnoreCase("INTEGER") ||
                dataType.equalsIgnoreCase("SMALLINT") ||
                dataType.equalsIgnoreCase("BIGINT") ||
                dataType.equalsIgnoreCase("FLOAT") ||
                dataType.equalsIgnoreCase("REAL") ||
                dataType.equalsIgnoreCase("DOUBLE");
    }
}
