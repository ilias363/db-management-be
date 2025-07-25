package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.repositories.PermissionRepository;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingPermissions;

import java.util.Collection;

@AllArgsConstructor
public class ExistingPermissionsValidator implements ConstraintValidator<ExistingPermissions, Collection<?>> {

    private PermissionRepository permissionRepository;

    @Override
    public boolean isValid(Collection<?> permissions, ConstraintValidatorContext context) {
        if (permissions == null || permissions.isEmpty()) {
            return true;
        }

        return ValidationUtils.validateEntitiesExist(
                permissions,
                permission -> {
                    Long permissionId = ValidationUtils.extractId(permission);
                    return permissionId != null && permissionRepository.existsById(permissionId);
                },
                null,
                null);
    }
}
