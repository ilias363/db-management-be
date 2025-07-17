package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.NewStandardColumnDto;

public class NotNullOrUniqueValidator implements ConstraintValidator<NotNullOrUnique, NewStandardColumnDto> {

    @Override
    public boolean isValid(NewStandardColumnDto dto, ConstraintValidatorContext context) {
        if (dto == null) {
            return true;
        }
        return Boolean.TRUE.equals(dto.getIsNullable()) || Boolean.FALSE.equals(dto.getIsUnique());
    }
}
