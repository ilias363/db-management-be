package ma.ilias.dbmanagementbe.service;

import ma.ilias.dbmanagementbe.dto.appuser.*;

public interface AppUserService {
    AppUserDto save(NewAppUserDto newAppUserDto);

    AppUserDto findById(Long id);

    AppUserDto findByUsername(String username, boolean checkAuthorization);

    AppUserPageDto findAllPaginated(int page, int size, String sortBy, String sortDirection, String search);

    AppUserPageDto findAllActivePaginated(int page, int size, String sortBy, String sortDirection, String search);

    AppUserDto update(Long id, UpdateAppUserDto appUserDto);

    //    Boolean deleteById(Long id);

    void deactivateById(Long id);

    void activateById(Long id);

    AppUserDto getCurrentUserInfo();

    AppUserStatsDto getUserStats();
}
