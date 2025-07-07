package ma.ilias.dbmanagementbe.mapper.config;

import ma.ilias.dbmanagementbe.dao.entities.Role;
import ma.ilias.dbmanagementbe.dto.role.RoleDto;
import org.modelmapper.ModelMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ModelMapperConfig {
    @Bean
    public ModelMapper modelMapper() {
        ModelMapper modelMapper = new ModelMapper();

        modelMapper.typeMap(Role.class, RoleDto.class)
                .addMappings(mapper -> mapper.map(
                        Role::getPermissionsWithoutRole,
                        RoleDto::setPermissions
                ));

        return modelMapper;
    }
}
