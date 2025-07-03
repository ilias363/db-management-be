package ma.ilias.dbmanagementbe.mapper;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import ma.ilias.dbmanagementbe.dto.AppUserDto;
import ma.ilias.dbmanagementbe.dto.NewAppUserDto;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class AppUserMapper {
    private final ModelMapper modelMapper;

    public AppUserDto toDto(AppUser appUser) {
        return modelMapper.map(appUser, AppUserDto.class);
    }

    public AppUser toEntity(AppUserDto appUserDto) {
        return modelMapper.map(appUserDto, AppUser.class);
    }

    public AppUser toEntity(NewAppUserDto newAppUserDto) {
        return modelMapper.map(newAppUserDto, AppUser.class);
    }
}
