package ma.ilias.dbmanagementbe.service;

import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import javax.sql.DataSource;
import java.util.List;

@Service
public class TestTargetDbService {

    private final JdbcTemplate jdbcTemplate;
    private final AppUserRepository appUserRepository;

    public TestTargetDbService(@Qualifier("targetDatasource") DataSource dataSource,
                               AppUserRepository appUserRepository) {
        this.jdbcTemplate = new JdbcTemplate(dataSource);
        this.appUserRepository = appUserRepository;
    }

    public List<String> getDatabases() {
        return jdbcTemplate.queryForList("SHOW DATABASES", String.class);
    }

    public List<AppUser> getUsers() {
        return appUserRepository.findAll();
    }
}
