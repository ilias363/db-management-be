package ma.ilias.dbmanagementbe.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.Role;
import ma.ilias.dbmanagementbe.dao.repositories.PermissionRepository;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.dto.role.NewRoleDto;
import ma.ilias.dbmanagementbe.dto.role.RoleDto;
import ma.ilias.dbmanagementbe.dto.role.UpdateRoleDto;
import ma.ilias.dbmanagementbe.exception.PermissionNotFoundException;
import ma.ilias.dbmanagementbe.exception.RoleNotFoundException;
import ma.ilias.dbmanagementbe.mapper.RoleMapper;
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

    @Override
    public RoleDto save(NewRoleDto newRoleDto) {
        Role role = new Role();
        role.setName(newRoleDto.getName());
        role.setDescription(newRoleDto.getDescription());
        role.setIsSystemRole(false);
        role.setPermissions(
                newRoleDto.getPermissions().stream()
                        .map( permissionId -> permissionRepository.findById(permissionId)
                                .orElseThrow(() -> new PermissionNotFoundException("Permission not found with ID: " + permissionId)))
                        .collect(Collectors.toSet())
        );

        return roleMapper.toDto(roleRepository.save(role));
    }

    @Override
    public RoleDto findById(Long id) {
        Role role = roleRepository.findById(id)
                .orElseThrow(() -> new RoleNotFoundException("Role not found with ID: " + id));
        return roleMapper.toDto(role);
    }

    @Override
    public List<RoleDto> findAll() {
        return roleRepository.findAll().stream()
                .map(roleMapper::toDto)
                .collect(Collectors.toList());
    }

    @Override
    public RoleDto update(Long id, UpdateRoleDto updateRoleDto) {
        if (!Objects.equals(id, updateRoleDto.getId())) {
            throw new RuntimeException(
                    "Path variable ID=" + id + " does not match request body entity ID=" + updateRoleDto.getId()
            );
        }

        Role existingRole = roleRepository.findById(id)
                .orElseThrow(() -> new RoleNotFoundException("Role not found with ID: " + id));

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
}
