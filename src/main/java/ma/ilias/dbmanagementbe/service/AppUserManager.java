package ma.ilias.dbmanagementbe.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.dto.appuser.AppUserDto;
import ma.ilias.dbmanagementbe.dto.appuser.NewAppUserDto;
import ma.ilias.dbmanagementbe.dto.appuser.UpdateAppUserDto;
import ma.ilias.dbmanagementbe.mapper.AppUserMapper;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import ma.ilias.dbmanagementbe.exception.RoleNotFoundException;
import ma.ilias.dbmanagementbe.exception.UserNotFoundException;
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
        AppUser appUser = appUserMapper.toEntity(newAppUserDto);
        appUser.setPasswordHash(passwordEncoder.encode(newAppUserDto.getPassword()));
        appUser.setActive(newAppUserDto.getActive());
        appUser.setRoles(newAppUserDto.getRoles().stream()
                .map(roleId -> roleRepository.findById(roleId)
                        .orElseThrow(() -> new RoleNotFoundException("Role not found with ID: " + roleId)))
                .collect(Collectors.toList()));
        AppUser savedAppUser = appUserRepository.save(appUser);
        return appUserMapper.toDto(savedAppUser);
    }

    @Override
    public AppUserDto findById(Long id) {
        AppUser appUser = appUserRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found with ID: " + id));
        return appUserMapper.toDto(appUser);
    }

    @Override
    public AppUserDto findByUsername(String username) {
        AppUser appUser = appUserRepository.findByUsername(username)
                .orElseThrow(() -> new UserNotFoundException("User not found with username: " + username));
        return appUserMapper.toDto(appUser);
    }

    @Override
    public List<AppUserDto> findAll() {
        return appUserRepository.findAll().stream()
                .map(appUserMapper::toDto)
                .collect(Collectors.toList());
    }

    @Override
    public AppUserDto update(Long id, UpdateAppUserDto updateAppUserDto) {
        AppUser existingAppUser = appUserRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found with ID: " + id));

        existingAppUser.setUsername(updateAppUserDto.getUsername());
        existingAppUser.setEmail(updateAppUserDto.getEmail());
        existingAppUser.setActive(updateAppUserDto.getActive());
        existingAppUser.setRoles(updateAppUserDto.getRoles().stream()
                .map(roleId -> roleRepository.findById(roleId)
                        .orElseThrow(() -> new RoleNotFoundException("Role not found with ID: " + roleId)))
                .collect(Collectors.toList()));

        AppUser updatedAppUser = appUserRepository.save(existingAppUser);
        return appUserMapper.toDto(updatedAppUser);
    }

    @Override
    public Boolean deleteById(Long id) {
        if (!appUserRepository.existsById(id)) {
            throw new UserNotFoundException("User not found with ID: " + id);
        }
        appUserRepository.deleteById(id);
        return !appUserRepository.existsById(id);
    }
}