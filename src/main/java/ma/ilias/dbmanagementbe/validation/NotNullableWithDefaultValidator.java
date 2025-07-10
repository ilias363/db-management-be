package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.column.UpdateColumnNullableDto;

public class NotNullableWithDefaultValidator implements ConstraintValidator<NotNullableWithDefault, UpdateColumnNullableDto> {

    @Override
    public boolean isValid(UpdateColumnNullableDto dto, ConstraintValidatorContext context) {
        if (dto == null) {
            return true; // Let other validators handle null DTOs
        }

        // If isNullable is true, we are allowing nulls, so no default value is needed.
        if (Boolean.TRUE.equals(dto.getIsNullable())) {
            return true;
        }

        // If isNullable is false, we are adding a NOT NULL constraint.
        // In this case, a default value must be present to handle existing nulls.
        if (Boolean.FALSE.equals(dto.getIsNullable())) {
            return dto.getDefaultValue() != null && !dto.getDefaultValue().trim().isEmpty();
        }

        return true;
    }
}
