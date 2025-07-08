package ma.ilias.dbmanagementbe.service;

import ma.ilias.dbmanagementbe.dto.permission.NewPermissionDto;
import ma.ilias.dbmanagementbe.dto.permission.PermissionDto;
import ma.ilias.dbmanagementbe.dto.permission.UpdatePermissionDto;

import java.util.List;

public interface PermissionService {
    PermissionDto save(NewPermissionDto newPermissionDto);
    List<PermissionDto> findAll();
    PermissionDto findById(Long id);
    List<PermissionDto> findByPermissionType(String type);
    PermissionDto update(Long id, UpdatePermissionDto updatePermissionDto);
    Boolean deleteById(Long id);
}