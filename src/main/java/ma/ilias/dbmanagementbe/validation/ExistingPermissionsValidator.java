package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.dao.repositories.PermissionRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Collection;

public class ExistingPermissionsValidator implements ConstraintValidator<ExistingPermissions, Collection<Long>> {

    @Autowired
    private PermissionRepository permissionRepository;

    @Override
    public boolean isValid(Collection<Long> permissionIds, ConstraintValidatorContext context) {
        if (permissionIds == null || permissionIds.isEmpty()) {
            return true;
        }
        return permissionIds.stream().allMatch(permissionRepository::existsById);
    }
}
