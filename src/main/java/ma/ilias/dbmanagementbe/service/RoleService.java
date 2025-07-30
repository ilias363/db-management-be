package ma.ilias.dbmanagementbe.service;

import ma.ilias.dbmanagementbe.dto.role.NewRoleDto;
import ma.ilias.dbmanagementbe.dto.role.RoleDto;
import ma.ilias.dbmanagementbe.dto.role.RolePageDto;
import ma.ilias.dbmanagementbe.dto.role.UpdateRoleDto;

public interface RoleService {
    RoleDto save(NewRoleDto newRoleDto);

    RoleDto findById(Long id);

    RolePageDto findAllPaginated(int page, int size, String sortBy, String sortDirection, String search);

    RoleDto update(Long id, UpdateRoleDto updateRoleDto);

    Boolean deleteById(Long id);
}
