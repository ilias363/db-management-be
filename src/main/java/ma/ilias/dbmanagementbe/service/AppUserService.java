package ma.ilias.dbmanagementbe.service;

import ma.ilias.dbmanagementbe.dto.AppUserDto;
import ma.ilias.dbmanagementbe.dto.NewAppUserDto;
import ma.ilias.dbmanagementbe.dto.UpdateAppUserDto;

import java.util.List;

public interface AppUserService {
    AppUserDto save(NewAppUserDto newAppUserDto);
    AppUserDto findById(Long id);
    AppUserDto findByUsername(String username);
    List<AppUserDto> findAll();
    AppUserDto update(Long id, UpdateAppUserDto appUserDto);
    Boolean deleteById(Long id);
}