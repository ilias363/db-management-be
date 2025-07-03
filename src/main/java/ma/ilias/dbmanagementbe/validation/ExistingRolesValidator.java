package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Collection;

public class ExistingRolesValidator implements ConstraintValidator<ExistingRoles, Collection<Long>> {

    @Autowired
    private RoleRepository roleRepository;

    @Override
    public boolean isValid(Collection<Long> roleIds, ConstraintValidatorContext context) {
        if (roleIds == null || roleIds.isEmpty()) {
            return true; // Handled by @NotEmpty
        }
        return roleIds.stream().allMatch(roleRepository::existsById);
    }
}
