package ma.ilias.dbmanagementbe.mapper;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.Permission;
import ma.ilias.dbmanagementbe.dto.appuser.NewAppUserDto;
import ma.ilias.dbmanagementbe.dto.permission.NewPermissionDto;
import ma.ilias.dbmanagementbe.dto.permission.PermissionDto;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class PermissionMapper {
    private final ModelMapper modelMapper;

    public PermissionDto toDto(Permission permission) {
        return modelMapper.map(permission, PermissionDto.class);
    }

    public Permission toEntity(PermissionDto permissionDto) {
        return modelMapper.map(permissionDto, Permission.class);
    }

    public Permission toEntity(NewPermissionDto newPermissionDto) {
        return modelMapper.map(newPermissionDto, Permission.class);
    }
}
