package ma.ilias.dbmanagementbe.database.service;

import ma.ilias.dbmanagementbe.database.dto.DatabaseStatsDto;
import ma.ilias.dbmanagementbe.enums.DatabaseType;

public interface DatabaseInfoService {
    DatabaseType getDatabaseType();
    DatabaseStatsDto getStats(boolean includeSystemSchemas);
}
