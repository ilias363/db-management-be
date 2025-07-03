package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import org.springframework.beans.factory.annotation.Autowired;

public class UniqueRoleNameValidator implements ConstraintValidator<UniqueRoleName, String> {

    @Autowired
    private RoleRepository roleRepository;

    @Override
    public boolean isValid(String roleName, ConstraintValidatorContext context) {
        if (roleName == null) {
            return true; // Let @NotBlank handle this
        }
        return roleRepository.findByName(roleName).isEmpty();
    }
}
