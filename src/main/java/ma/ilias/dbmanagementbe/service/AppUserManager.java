package ma.ilias.dbmanagementbe.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.dto.appuser.AppUserDto;
import ma.ilias.dbmanagementbe.dto.appuser.NewAppUserDto;
import ma.ilias.dbmanagementbe.dto.appuser.UpdateAppUserDto;
import ma.ilias.dbmanagementbe.exception.InsufficientPermissionException;
import ma.ilias.dbmanagementbe.exception.RoleNotFoundException;
import ma.ilias.dbmanagementbe.exception.UserNotFoundException;
import ma.ilias.dbmanagementbe.mapper.AppUserMapper;
import ma.ilias.dbmanagementbe.util.AuthorizationUtils;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
@Transactional
public class AppUserManager implements AppUserService {

    private final AppUserRepository appUserRepository;
    private final AppUserMapper appUserMapper;
    private final RoleRepository roleRepository;
    private final PasswordEncoder passwordEncoder;

    @Override
    public AppUserDto save(NewAppUserDto newAppUserDto) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can create users");
        }

        AppUser appUser = appUserMapper.toEntity(newAppUserDto);
        appUser.setPassword(passwordEncoder.encode(newAppUserDto.getPassword()));
        appUser.setActive(newAppUserDto.getActive());
        appUser.setRoles(newAppUserDto.getRoles().stream()
                .map(roleId -> roleRepository.findById(roleId)
                        .orElseThrow(() -> new RoleNotFoundException("Role not found with ID: " + roleId)))
                .collect(Collectors.toSet()));
        AppUser savedAppUser = appUserRepository.save(appUser);
        return appUserMapper.toDto(savedAppUser);
    }

    @Override
    public AppUserDto findById(Long id) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can view user details");
        }

        AppUser appUser = appUserRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found with ID: " + id));
        return appUserMapper.toDto(appUser);
    }

    @Override
    public AppUserDto findByUsername(String username) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can view user details");
        }

        AppUser appUser = appUserRepository.findByUsername(username)
                .orElseThrow(() -> new UserNotFoundException("User not found with username: " + username));
        return appUserMapper.toDto(appUser);
    }

    @Override
    public List<AppUserDto> findAll() {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can view users");
        }

        return appUserRepository.findAll().stream()
                .map(appUserMapper::toDto)
                .collect(Collectors.toList());
    }

    @Override
    public List<AppUserDto> findAllActive() {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can view users");
        }

        return appUserRepository.findByActive(true).stream()
                .map(appUserMapper::toDto)
                .collect(Collectors.toList());
    }

    @Override
    public AppUserDto update(Long id, UpdateAppUserDto updateAppUserDto) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can update users");
        }

        if (!Objects.equals(id, updateAppUserDto.getId())) {
            throw new RuntimeException(
                    "Path variable ID=" + id + " does not match request body entity ID=" + updateAppUserDto.getId()
            );
        }

        AppUser existingAppUser = appUserRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found with ID: " + id));

        existingAppUser.setUsername(updateAppUserDto.getUsername());
        existingAppUser.setActive(updateAppUserDto.getActive());
        existingAppUser.setRoles(updateAppUserDto.getRoles().stream()
                .map(roleId -> roleRepository.findById(roleId)
                        .orElseThrow(() -> new RoleNotFoundException("Role not found with ID: " + roleId)))
                .collect(Collectors.toSet()));

        AppUser updatedAppUser = appUserRepository.save(existingAppUser);
        return appUserMapper.toDto(updatedAppUser);
    }

//    @Override
//    public Boolean deleteById(Long id) {
//        if (!AuthorizationUtils.hasUserManagementAccess()) {
//            throw new InsufficientPermissionException("Only administrators can delete users");
//        }
//
//        if (!appUserRepository.existsById(id)) {
//            throw new UserNotFoundException("User not found with ID: " + id);
//        }
//
//        appUserRepository.deleteById(id);
//        return !appUserRepository.existsById(id);
//    }

    @Override
    public void deactivateById(Long id) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can deactivate users");
        }

        AppUser user = appUserRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found with ID: " + id));

        user.setActive(false);
        appUserRepository.save(user);
    }

    @Override
    public void activateById(Long id) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can activate users");
        }

        AppUser user = appUserRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found with ID: " + id));

        user.setActive(true);
        appUserRepository.save(user);
    }

    @Override
    public AppUserDto getCurrentUserInfo() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.isAuthenticated() &&
                !"anonymousUser".equals(authentication.getPrincipal())) {

            if (authentication.getPrincipal() instanceof AppUser) {
                return appUserMapper.toDto((AppUser) authentication.getPrincipal());
            } else if (authentication.getName() != null) {
                return appUserMapper.toDto(
                        appUserRepository.findByUsername(authentication.getName()).orElse(null));
            }
        }
        return null;
    }
}