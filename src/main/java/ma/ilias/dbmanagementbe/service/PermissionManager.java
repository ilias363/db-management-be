package ma.ilias.dbmanagementbe.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.Permission;
import ma.ilias.dbmanagementbe.dao.repositories.PermissionRepository;
import ma.ilias.dbmanagementbe.dto.permission.NewPermissionDto;
import ma.ilias.dbmanagementbe.dto.permission.PermissionDto;
import ma.ilias.dbmanagementbe.dto.permission.UpdatePermissionDto;
import ma.ilias.dbmanagementbe.exception.PermissionNotFoundException;
import ma.ilias.dbmanagementbe.mapper.PermissionMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import ma.ilias.dbmanagementbe.enums.PermissionType;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
@Transactional
public class PermissionManager implements PermissionService {

    private final PermissionRepository permissionRepository;
    private final PermissionMapper permissionMapper;

    @Override
    public PermissionDto save(NewPermissionDto newPermissionDto) {
        Permission permission = new Permission();
        permission.setSchemaName(newPermissionDto.getSchemaName());
        permission.setTableName(newPermissionDto.getTableName());
        permission.setPermissionType(PermissionType.valueOf(newPermissionDto.getPermissionType()));
        Permission savedPermission = permissionRepository.save(permission);
        return permissionMapper.toDto(savedPermission);
    }

    @Override
    public List<PermissionDto> findAll() {
        return permissionRepository.findAll().stream()
                .map(permissionMapper::toDto)
                .collect(Collectors.toList());
    }

    @Override
    public PermissionDto findById(Long id) {
        Permission permission = permissionRepository.findById(id)
                .orElseThrow(() -> new PermissionNotFoundException("Permission not found with ID: " + id));
        return permissionMapper.toDto(permission);
    }

    @Override
    public List<PermissionDto> findByPermissionType(String type) {
        try {
            return permissionRepository.findByPermissionType(PermissionType.valueOf(type))
                    .stream()
                    .map(permissionMapper::toDto)
                    .collect(Collectors.toList());
        } catch (IllegalArgumentException e) {
            throw new RuntimeException("Type does not exist");
        }
    }

    @Override
    public PermissionDto update(Long id, UpdatePermissionDto updatePermissionDto) {
        if (!Objects.equals(id, updatePermissionDto.getId())) {
            throw new RuntimeException(
                    "Path variable ID=" + id + " does not match request body entity ID=" + updatePermissionDto.getId()
            );
        }

        Permission existingPermission = permissionRepository.findById(id)
                .orElseThrow(() -> new PermissionNotFoundException("Permission not found with ID: " + id));

        existingPermission.setSchemaName(updatePermissionDto.getSchemaName());
        existingPermission.setTableName(updatePermissionDto.getTableName());
        existingPermission.setPermissionType(
                PermissionType.valueOf(updatePermissionDto.getPermissionType())
        );

        Permission updatedPermission = permissionRepository.save(existingPermission);
        return permissionMapper.toDto(updatedPermission);
    }
}
