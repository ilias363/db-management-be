package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.record.dto.FilterCriteriaDto;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ValidFilterOperator;

public class ValidFilterOperatorValidator implements ConstraintValidator<ValidFilterOperator, String> {

    @Override
    public boolean isValid(String value, ConstraintValidatorContext context) {
        if (value == null) {
            return true; // @NotNull will handle null checks
        }

        return ValidationUtils.validateEnum(value, FilterCriteriaDto.FilterOperator.class);
    }
}
