package ma.ilias.dbmanagementbe.service;

import ma.ilias.dbmanagementbe.dto.role.NewRoleDto;
import ma.ilias.dbmanagementbe.dto.role.RoleDto;
import ma.ilias.dbmanagementbe.dto.role.UpdateRoleDto;

import java.util.List;

public interface RoleService {
    RoleDto save(NewRoleDto newRoleDto);
    RoleDto findById(Long id);
    List<RoleDto> findAll();
    RoleDto update(Long id, UpdateRoleDto updateRoleDto);
}