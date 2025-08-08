package ma.ilias.dbmanagementbe.database.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.database.dto.DatabaseStatsDto;
import ma.ilias.dbmanagementbe.enums.DatabaseType;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
@Transactional(readOnly = true)
public class MySqlDatabaseInfoManager implements DatabaseInfoService {

    private final JdbcTemplate targetJdbcTemplate;
    private final MetadataProviderService metadataProviderService;
    private final DatabaseType databaseType;

    @Override
    public DatabaseType getDatabaseType() {
        return databaseType;
    }

    @Override
    public DatabaseStatsDto getStats(boolean includeSystemSchemas) {
        long totalSchemas = metadataProviderService.getAllSchemas(includeSystemSchemas, false, false).size();

        String tableCountSql = "SELECT COUNT(*) FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE = 'BASE TABLE'" +
                (includeSystemSchemas ? "" : " AND TABLE_SCHEMA NOT IN ('mysql','sys','information_schema','performance_schema')");
        long totalTables = targetJdbcTemplate.queryForObject(tableCountSql, Long.class);

        String viewCountSql = "SELECT COUNT(*) FROM INFORMATION_SCHEMA.VIEWS" +
                (includeSystemSchemas ? "" : " WHERE TABLE_SCHEMA NOT IN ('mysql','sys','information_schema','performance_schema')");
        long totalViews = targetJdbcTemplate.queryForObject(viewCountSql, Long.class);

        String recordCountSql = "SELECT SUM(TABLE_ROWS) FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE = 'BASE TABLE'" +
                (includeSystemSchemas ? "" : " AND TABLE_SCHEMA NOT IN ('mysql','sys','information_schema','performance_schema')");
        Long totalRecordsObj = targetJdbcTemplate.queryForObject(recordCountSql, Long.class);
        long totalRecords = totalRecordsObj == null ? 0L : totalRecordsObj;

        return DatabaseStatsDto.builder()
                .totalSchemas(totalSchemas)
                .totalTables(totalTables)
                .totalViews(totalViews)
                .totalRecords(totalRecords)
                .build();
    }
}
