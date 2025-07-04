package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.enums.PermissionType;

public class ValidPermissionTypeValidator implements ConstraintValidator<ValidPermissionType, String> {

    @Override
    public boolean isValid(String value, ConstraintValidatorContext context) {
        if (value == null) {
            return true; // @NotNull will handle this
        }
        for (PermissionType permissionType : PermissionType.values()) {
            if (permissionType.name().equals(value)) {
                return true;
            }
        }
        return false;
    }
}
