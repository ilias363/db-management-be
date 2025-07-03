package ma.ilias.dbmanagementbe.config;

import com.zaxxer.hikari.HikariDataSource;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

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
}
