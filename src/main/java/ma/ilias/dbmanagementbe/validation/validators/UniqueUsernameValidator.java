package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dto.appuser.AppUserDtoBase;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueUsername;
import org.springframework.beans.factory.annotation.Autowired;

public class UniqueUsernameValidator implements ConstraintValidator<UniqueUsername, AppUserDtoBase> {

    @Autowired
    private AppUserRepository appUserRepository;

    @Override
    public boolean isValid(AppUserDtoBase dto, ConstraintValidatorContext context) {
        if (dto.getUsername() == null) {
            return true; // Let @NotBlank handle this
        }

        return ValidationUtils.validateUniqueness(
                appUserRepository,
                dto.getUsername(),
                dto.getId(),
                "findByUsername");
    }
}
