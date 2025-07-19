package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.dao.entities.Role;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.dto.role.RoleDtoBase;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueRoleName;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Optional;

public class UniqueRoleNameValidator implements ConstraintValidator<UniqueRoleName, RoleDtoBase> {

    @Autowired
    private RoleRepository roleRepository;

    @Override
    public boolean isValid(RoleDtoBase dto, ConstraintValidatorContext context) {
        if (dto.getName() == null) {
            return true; // Let @NotBlank handle this
        }
        // for NewAppUserDto
        if (dto.getId() == null) {
            return roleRepository.findByName(dto.getName()).isEmpty();
        }
        // for UpdateAppUserDto
        Optional<Role> result = roleRepository.findByName(dto.getName());
        return result.map(appUser -> appUser.getId().equals(dto.getId())).orElse(true);
    }
}
