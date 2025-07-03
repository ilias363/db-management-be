package ma.ilias.dbmanagementbe.mapper;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.Role;
import ma.ilias.dbmanagementbe.dto.NewAppUserDto;
import ma.ilias.dbmanagementbe.dto.RoleDto;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class RoleMapper {
    private final ModelMapper modelMapper;

    public RoleDto toDto(Role role) {
        return modelMapper.map(role, RoleDto.class);
    }

    public Role toEntity(RoleDto roleDto) {
        return modelMapper.map(roleDto, Role.class);
    }

    public Role toEntity(NewAppUserDto newAppUserDto) {
        return modelMapper.map(newAppUserDto, Role.class);
    }
}
