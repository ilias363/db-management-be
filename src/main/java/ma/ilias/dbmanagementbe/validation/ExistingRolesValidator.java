package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.dto.role.RoleDto;
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

        for (Object role : roles) {
            Long roleId;
            if (role instanceof Long) {
                roleId = (Long) role;
            } else if (role instanceof RoleDto) {
                roleId = ((RoleDto) role).getId();
            } else {
                return false;
            }

            if (!roleRepository.existsById(roleId)) {
                return false;
            }
        }
        return true;
    }
}
