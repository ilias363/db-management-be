package ma.ilias.dbmanagementbe.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.Permission;
import ma.ilias.dbmanagementbe.dao.entities.Role;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.dto.role.*;
import ma.ilias.dbmanagementbe.enums.PermissionType;
import ma.ilias.dbmanagementbe.enums.SystemRole;
import ma.ilias.dbmanagementbe.exception.InsufficientPermissionException;
import ma.ilias.dbmanagementbe.exception.RoleNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.mapper.RoleMapper;
import ma.ilias.dbmanagementbe.util.AuthorizationUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
@Transactional
public class RoleManager implements RoleService {

    private final RoleRepository roleRepository;
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

        // Create permissions and associate them with the role
        Set<Permission> permissions = newRoleDto.getPermissions().stream()
                .map(permissionDetail -> {
                    Permission permission = new Permission();
                    permission.setSchemaName(permissionDetail.getSchemaName());
                    permission.setTableName(permissionDetail.getTableName());
                    permission.setViewName(permissionDetail.getViewName());
                    permission.setPermissionType(PermissionType.valueOf(permissionDetail.getPermissionType()));
                    permission.setRole(role);  // Set the role relationship
                    return permission;
                })
                .collect(Collectors.toSet());

        role.setPermissions(permissions);

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

        List<Role> roles = roleRepository.findAll();
        return roles
                .stream()
                .map(roleMapper::toDto)
                .toList();
    }

    @Override
    public RolePageDto findAllPaginated(int page, int size, String sortBy, String sortDirection, String search) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can view roles");
        }

        Sort sort = createSort(sortBy, sortDirection);
        Pageable pageable = PageRequest.of(page, size, sort);

        Page<Role> rolePage = roleRepository.findAllWithSearch(search, pageable);

        return RolePageDto.builder()
                .items(rolePage.getContent().stream()
                        .map(roleMapper::toDto)
                        .toList())
                .totalItems(rolePage.getTotalElements())
                .currentPage(page)
                .pageSize(size)
                .totalPages(rolePage.getTotalPages())
                .build();
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

        // Clear existing permissions (orphan removal will delete them)
        existingRole.getPermissions().clear();

        // Create new permissions and associate them with the role
        Set<Permission> newPermissions = updateRoleDto.getPermissions().stream()
                .map(permissionDetail -> {
                    Permission permission = new Permission();
                    permission.setSchemaName(permissionDetail.getSchemaName());
                    permission.setTableName(permissionDetail.getTableName());
                    permission.setViewName(permissionDetail.getViewName());
                    permission.setPermissionType(PermissionType.valueOf(permissionDetail.getPermissionType()));
                    permission.setRole(existingRole);  // Set the role relationship
                    return permission;
                })
                .collect(Collectors.toSet());

        existingRole.getPermissions().addAll(newPermissions);

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

    @Override
    public RoleStatsDto getRoleStats() {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can view role statistics");
        }

        long totalRoles = roleRepository.count();
        long systemRoles = roleRepository.countSystemRoles();
        long customRoles = totalRoles - systemRoles;
        long roleAssignments = roleRepository.countRoleAssignments();

        return RoleStatsDto.builder()
                .totalRoles(totalRoles)
                .systemRoles(systemRoles)
                .customRoles(customRoles)
                .roleAssignments(roleAssignments)
                .build();
    }

    private Sort createSort(String sortBy, String sortDirection) {
        final List<String> validFields = List.of(
                "id", "name", "description", "isSystemRole"
        );

        if (sortBy == null || sortBy.isBlank() || !validFields.contains(sortBy)) {
            sortBy = "name";
        }

        Sort.Direction direction = Sort.Direction.ASC;
        if ("DESC".equalsIgnoreCase(sortDirection)) {
            direction = Sort.Direction.DESC;
        }

        return Sort.by(direction, sortBy);
    }
}
