package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dto.appuser.AppUserDtoBase;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Optional;

public class UniqueUsernameValidator implements ConstraintValidator<UniqueUsername, AppUserDtoBase> {

    @Autowired
    private AppUserRepository appUserRepository;

    @Override
    public boolean isValid(AppUserDtoBase dto, ConstraintValidatorContext context) {
        if (dto.getUsername() == null) {
            return true; // Let @NotBlank handle this
        }
        // for NewAppUserDto
        if (dto.getId() == null) {
            return appUserRepository.findByUsername(dto.getUsername()).isEmpty();
        }
        // for UpdateAppUserDto
        Optional<AppUser> result = appUserRepository.findByUsername(dto.getUsername());
        return result.map(appUser -> appUser.getId().equals(dto.getId())).orElse(true);
    }
}
