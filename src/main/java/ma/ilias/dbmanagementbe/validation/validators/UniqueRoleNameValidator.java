package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.dto.role.RoleDtoBase;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueRoleName;
import org.springframework.beans.factory.annotation.Autowired;

public class UniqueRoleNameValidator implements ConstraintValidator<UniqueRoleName, RoleDtoBase> {

    @Autowired
    private RoleRepository roleRepository;

    @Override
    public boolean isValid(RoleDtoBase dto, ConstraintValidatorContext context) {
        if (dto.getName() == null) {
            return true; // Let @NotBlank handle this
        }

        return ValidationUtils.validateUniqueness(
                roleRepository,
                dto.getName(),
                dto.getId(),
                "findByName");
    }
}
