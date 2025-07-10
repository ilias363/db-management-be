package ma.ilias.dbmanagementbe.config;

import com.zaxxer.hikari.HikariDataSource;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;

import javax.sql.DataSource;

@Configuration
public class DatabaseConfig {

    @Qualifier("targetDatasource")
    @Bean(defaultCandidate = false)
    @ConfigurationProperties("app.datasource")
    public DataSourceProperties targetDataSourceProperties() {
        return new DataSourceProperties();
    }

    @Qualifier("targetDatasource")
    @Bean(defaultCandidate = false)
    @ConfigurationProperties("app.datasource.configuration")
    public HikariDataSource targetDataSource(
            @Qualifier("targetDataSourceProperties") DataSourceProperties targetDataSourceProperties) {
        return targetDataSourceProperties.initializeDataSourceBuilder().type(HikariDataSource.class).build();
    }

    @Bean
    public JdbcTemplate targetJdbcTemplate(@Qualifier("targetDatasource") DataSource dataSource) {
        return new JdbcTemplate(dataSource);
    }
}
