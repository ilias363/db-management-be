package ma.ilias.dbmanagementbe.service;

import ma.ilias.dbmanagementbe.dto.appuser.AppUserDto;
import ma.ilias.dbmanagementbe.dto.appuser.NewAppUserDto;
import ma.ilias.dbmanagementbe.dto.appuser.UpdateAppUserDto;

import java.util.List;

public interface AppUserService {
    AppUserDto save(NewAppUserDto newAppUserDto);
    AppUserDto findById(Long id);
    AppUserDto findByUsername(String username);
    List<AppUserDto> findAll();
    AppUserDto update(Long id, UpdateAppUserDto appUserDto);
    Boolean deleteById(Long id);
}