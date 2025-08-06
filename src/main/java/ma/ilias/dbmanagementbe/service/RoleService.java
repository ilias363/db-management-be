package ma.ilias.dbmanagementbe.service;

import ma.ilias.dbmanagementbe.dto.role.*;

import java.util.List;

public interface RoleService {
    RoleDto save(NewRoleDto newRoleDto);

    RoleDto findById(Long id);

    List<RoleDto> findAll();

    RolePageDto findAllPaginated(int page, int size, String sortBy, String sortDirection, String search);

    RoleDto update(Long id, UpdateRoleDto updateRoleDto);

    Boolean deleteById(Long id);

    RoleStatsDto getRoleStats();
}
