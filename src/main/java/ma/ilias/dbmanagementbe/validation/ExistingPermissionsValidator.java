package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.dao.repositories.PermissionRepository;
import ma.ilias.dbmanagementbe.dto.permission.PermissionDto;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Collection;

public class ExistingPermissionsValidator implements ConstraintValidator<ExistingPermissions, Collection<?>> {

    @Autowired
    private PermissionRepository permissionRepository;

    @Override
    public boolean isValid(Collection<?> permissions, ConstraintValidatorContext context) {
        if (permissions == null || permissions.isEmpty()) {
            return true;
        }

        for (Object permission : permissions) {
            Long permissionId;
            if (permission instanceof Long) {
                permissionId = (Long) permission;
            } else if (permission instanceof PermissionDto) {
                permissionId = ((PermissionDto) permission).getId();
            } else {
                return false;
            }

            if (!permissionRepository.existsById(permissionId)) {
                return false;
            }
        }
        return true;
    }
}
