package ma.ilias.dbmanagementbe.service;

import ma.ilias.dbmanagementbe.dto.role.*;

public interface RoleService {
    RoleDto save(NewRoleDto newRoleDto);

    RoleDto findById(Long id);

    RolePageDto findAllPaginated(int page, int size, String sortBy, String sortDirection, String search);

    RoleDto update(Long id, UpdateRoleDto updateRoleDto);

    Boolean deleteById(Long id);

    RoleStatsDto getRoleStats();
}
