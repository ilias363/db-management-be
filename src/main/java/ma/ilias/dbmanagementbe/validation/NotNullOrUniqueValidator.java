package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;

public class NotNullOrUniqueValidator implements ConstraintValidator<NotNullOrUnique, BaseNewColumnDto> {

    @Override
    public boolean isValid(BaseNewColumnDto dto, ConstraintValidatorContext context) {
        if (dto == null) {
            return true;
        }
        return Boolean.TRUE.equals(dto.getIsNullable()) || Boolean.FALSE.equals(dto.getIsUnique());
    }
}
