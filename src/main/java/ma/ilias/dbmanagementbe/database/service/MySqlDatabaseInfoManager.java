package ma.ilias.dbmanagementbe.database.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.database.dto.DatabaseStatsDto;
import ma.ilias.dbmanagementbe.enums.DatabaseType;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
@Transactional(readOnly = true)
public class MySqlDatabaseInfoManager implements DatabaseInfoService {

    private final JdbcTemplate jdbcTemplate;
    private final DatabaseType databaseType;

    @Override
    public DatabaseType getDatabaseType() {
        return databaseType;
    }

    @Override
    public DatabaseStatsDto getStats(boolean includeSystem) {
        String schemaCountSql = "SELECT COUNT(*) FROM INFORMATION_SCHEMA.SCHEMATA" +
                (includeSystem ? "" : " WHERE SCHEMA_NAME NOT IN ('mysql','sys','information_schema','performance_schema')");
        Long totalSchemas = jdbcTemplate.queryForObject(schemaCountSql, Long.class);

        String tableCountSql = "SELECT COUNT(*) FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE = 'BASE TABLE'" +
                (includeSystem ? "" : " AND TABLE_SCHEMA NOT IN ('mysql','sys','information_schema','performance_schema')");
        Long totalTables = jdbcTemplate.queryForObject(tableCountSql, Long.class);

        String viewCountSql = "SELECT COUNT(*) FROM INFORMATION_SCHEMA.VIEWS" +
                (includeSystem ? "" : " WHERE TABLE_SCHEMA NOT IN ('mysql','sys','information_schema','performance_schema')");
        Long totalViews = jdbcTemplate.queryForObject(viewCountSql, Long.class);

        String recordCountSql = "SELECT SUM(TABLE_ROWS) FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE = 'BASE TABLE'" +
                (includeSystem ? "" : " AND TABLE_SCHEMA NOT IN ('mysql','sys','information_schema','performance_schema')");
        Long totalRecords = jdbcTemplate.queryForObject(recordCountSql, Long.class);

        return DatabaseStatsDto.builder()
                .totalSchemas(totalSchemas != null ? totalSchemas : 0)
                .totalTables(totalTables != null ? totalTables : 0)
                .totalViews(totalViews != null ? totalViews : 0)
                .totalRecords(totalRecords != null ? totalRecords : 0)
                .build();
    }
}
