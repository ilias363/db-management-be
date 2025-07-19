package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingRoles;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Collection;

public class ExistingRolesValidator implements ConstraintValidator<ExistingRoles, Collection<?>> {

    @Autowired
    private RoleRepository roleRepository;

    @Override
    public boolean isValid(Collection<?> roles, ConstraintValidatorContext context) {
        if (roles == null || roles.isEmpty()) {
            return true;
        }

        return ValidationUtils.validateEntitiesExist(
                roles,
                role -> {
                    Long roleId = ValidationUtils.extractId(role);
                    return roleId != null && roleRepository.existsById(roleId);
                },
                null,
                null);
    }
}
