package ma.ilias.dbmanagementbe.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.Role;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dao.repositories.PermissionRepository;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.dto.role.NewRoleDto;
import ma.ilias.dbmanagementbe.dto.role.RoleDto;
import ma.ilias.dbmanagementbe.dto.role.UpdateRoleDto;
import ma.ilias.dbmanagementbe.enums.SystemRole;
import ma.ilias.dbmanagementbe.exception.InsufficientPermissionException;
import ma.ilias.dbmanagementbe.exception.PermissionNotFoundException;
import ma.ilias.dbmanagementbe.exception.RoleNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.mapper.RoleMapper;
import ma.ilias.dbmanagementbe.util.AuthorizationUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
@Transactional
public class RoleManager implements RoleService {

    private final RoleRepository roleRepository;
    private final PermissionRepository permissionRepository;
    private final RoleMapper roleMapper;
    private final AppUserRepository appUserRepository;

    @Override
    public RoleDto save(NewRoleDto newRoleDto) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can create roles");
        }

        if (SystemRole.ADMIN.name().equalsIgnoreCase(newRoleDto.getName().trim()) ||
                SystemRole.VIEWER.name().equalsIgnoreCase(newRoleDto.getName().trim())) {
            throw new UnauthorizedActionException("Cannot create role with system role name: " + newRoleDto.getName());
        }

        Role role = new Role();
        role.setName(newRoleDto.getName());
        role.setDescription(newRoleDto.getDescription());
        role.setIsSystemRole(false);
        role.setPermissions(
                newRoleDto.getPermissions().stream()
                        .map(permissionId -> permissionRepository.findById(permissionId)
                                .orElseThrow(() -> new PermissionNotFoundException("Permission not found with ID: " + permissionId)))
                        .collect(Collectors.toSet())
        );

        return roleMapper.toDto(roleRepository.save(role));
    }

    @Override
    public RoleDto findById(Long id) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can view role details");
        }

        Role role = roleRepository.findById(id)
                .orElseThrow(() -> new RoleNotFoundException("Role not found with ID: " + id));
        return roleMapper.toDto(role);
    }

    @Override
    public List<RoleDto> findAll() {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can view roles");
        }

        return roleRepository.findAll().stream()
                .map(roleMapper::toDto)
                .collect(Collectors.toList());
    }

    @Override
    public RoleDto update(Long id, UpdateRoleDto updateRoleDto) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can update roles");
        }

        if (!Objects.equals(id, updateRoleDto.getId())) {
            throw new RuntimeException(
                    "Path variable ID=" + id + " does not match request body entity ID=" + updateRoleDto.getId()
            );
        }

        Role existingRole = roleRepository.findById(id)
                .orElseThrow(() -> new RoleNotFoundException("Role not found with ID: " + id));

        if (existingRole.getIsSystemRole()) {
            throw new UnauthorizedActionException("Cannot modify system role: " + existingRole.getName());
        }

        if (SystemRole.ADMIN.name().equalsIgnoreCase(updateRoleDto.getName()) ||
                SystemRole.VIEWER.name().equalsIgnoreCase(updateRoleDto.getName())) {
            throw new UnauthorizedActionException("Cannot change role name to system role name: " + updateRoleDto.getName());
        }

        existingRole.setName(updateRoleDto.getName());
        existingRole.setDescription(updateRoleDto.getDescription());
        existingRole.setPermissions(
                updateRoleDto.getPermissions().stream()
                        .map(permissionId -> permissionRepository.findById(permissionId)
                                .orElseThrow(() -> new PermissionNotFoundException("Permission not found with ID: " + permissionId)))
                        .collect(Collectors.toSet())
        );
        Role updatedRole = roleRepository.save(existingRole);
        return roleMapper.toDto(updatedRole);
    }

    @Override
    public Boolean deleteById(Long id) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can delete roles");
        }

        Role role = roleRepository.findById(id)
                .orElseThrow(() -> new RoleNotFoundException("Role not found with ID: " + id));

        if (role.getIsSystemRole()) {
            throw new UnauthorizedActionException("Cannot delete system role: " + role.getName());
        }

        if (appUserRepository.existsByRolesId(id)) {
            throw new UnauthorizedActionException(
                    "Cannot delete role that is assigned to users. Remove the role from all users first."
            );
        }

        roleRepository.deleteById(id);
        return !roleRepository.existsById(id);
    }
}
