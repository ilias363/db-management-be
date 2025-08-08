package ma.ilias.dbmanagementbe.config;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.database.service.DatabaseInfoService;
import ma.ilias.dbmanagementbe.database.service.MySqlDatabaseInfoManager;
import ma.ilias.dbmanagementbe.enums.DatabaseType;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.metadata.service.MySqlMetadataProviderManager;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.metadata.service.column.MySqlColumnManager;
import ma.ilias.dbmanagementbe.metadata.service.index.IndexService;
import ma.ilias.dbmanagementbe.metadata.service.index.MySqlIndexManager;
import ma.ilias.dbmanagementbe.metadata.service.schema.MySqlSchemaManager;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaService;
import ma.ilias.dbmanagementbe.metadata.service.table.MySqlTableManager;
import ma.ilias.dbmanagementbe.metadata.service.table.TableService;
import ma.ilias.dbmanagementbe.metadata.service.view.MySqlViewManager;
import ma.ilias.dbmanagementbe.metadata.service.view.ViewService;
import ma.ilias.dbmanagementbe.record.service.MySqlRecordManager;
import ma.ilias.dbmanagementbe.record.service.RecordService;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

@Configuration
@AllArgsConstructor
public class ServiceSelectionConfig {

    private final DatabaseType databaseType;

    @Bean
    @Primary
    public MetadataProviderService metadataProviderService(MySqlMetadataProviderManager mySqlMetadataProviderService) {
        return switch (databaseType) {
            case MYSQL -> mySqlMetadataProviderService;
            case POSTGRESQL -> throw new IllegalArgumentException("PostgreSQL services are not implemented yet");
            case SQL_SERVER -> throw new IllegalArgumentException("SQL Server services are not implemented yet");
            case ORACLE -> throw new IllegalArgumentException("Oracle Database services are not implemented yet");
            case H2 -> throw new IllegalArgumentException("H2 Database services are not implemented yet");
            case SQLITE -> throw new IllegalArgumentException("SQLite services are not implemented yet");
        };
    }

    @Bean
    @Primary
    public SchemaService schemaService(MySqlSchemaManager mySqlSchemaManager) {
        return switch (databaseType) {
            case MYSQL -> mySqlSchemaManager;
            case POSTGRESQL -> throw new IllegalArgumentException("PostgreSQL services are not implemented yet");
            case SQL_SERVER -> throw new IllegalArgumentException("SQL Server services are not implemented yet");
            case ORACLE -> throw new IllegalArgumentException("Oracle Database services are not implemented yet");
            case H2 -> throw new IllegalArgumentException("H2 Database services are not implemented yet");
            case SQLITE -> throw new IllegalArgumentException("SQLite services are not implemented yet");
        };
    }

    @Bean
    @Primary
    public TableService tableService(MySqlTableManager mySqlTableManager) {
        return switch (databaseType) {
            case MYSQL -> mySqlTableManager;
            case POSTGRESQL -> throw new IllegalArgumentException("PostgreSQL services are not implemented yet");
            case SQL_SERVER -> throw new IllegalArgumentException("SQL Server services are not implemented yet");
            case ORACLE -> throw new IllegalArgumentException("Oracle Database services are not implemented yet");
            case H2 -> throw new IllegalArgumentException("H2 Database services are not implemented yet");
            case SQLITE -> throw new IllegalArgumentException("SQLite services are not implemented yet");
        };
    }

    @Bean
    @Primary
    public ViewService viewService(MySqlViewManager mySqlViewManager) {
        return switch (databaseType) {
            case MYSQL -> mySqlViewManager;
            case POSTGRESQL -> throw new IllegalArgumentException("PostgreSQL services are not implemented yet");
            case SQL_SERVER -> throw new IllegalArgumentException("SQL Server services are not implemented yet");
            case ORACLE -> throw new IllegalArgumentException("Oracle Database services are not implemented yet");
            case H2 -> throw new IllegalArgumentException("H2 Database services are not implemented yet");
            case SQLITE -> throw new IllegalArgumentException("SQLite services are not implemented yet");
        };
    }

    @Bean
    @Primary
    public ColumnService columnService(MySqlColumnManager mysqlcolumnManager) {
        return switch (databaseType) {
            case MYSQL -> mysqlcolumnManager;
            case POSTGRESQL -> throw new IllegalArgumentException("PostgreSQL services are not implemented yet");
            case SQL_SERVER -> throw new IllegalArgumentException("SQL Server services are not implemented yet");
            case ORACLE -> throw new IllegalArgumentException("Oracle Database services are not implemented yet");
            case H2 -> throw new IllegalArgumentException("H2 Database services are not implemented yet");
            case SQLITE -> throw new IllegalArgumentException("SQLite services are not implemented yet");
        };
    }

    @Bean
    @Primary
    public IndexService indexService(MySqlIndexManager mysqlindexManager) {
        return switch (databaseType) {
            case MYSQL -> mysqlindexManager;
            case POSTGRESQL -> throw new IllegalArgumentException("PostgreSQL services are not implemented yet");
            case SQL_SERVER -> throw new IllegalArgumentException("SQL Server services are not implemented yet");
            case ORACLE -> throw new IllegalArgumentException("Oracle Database services are not implemented yet");
            case H2 -> throw new IllegalArgumentException("H2 Database services are not implemented yet");
            case SQLITE -> throw new IllegalArgumentException("SQLite services are not implemented yet");
        };
    }

    @Bean
    @Primary
    public RecordService recordService(MySqlRecordManager mysqlRecordMananger) {
        return switch (databaseType) {
            case MYSQL -> mysqlRecordMananger;
            case POSTGRESQL -> throw new IllegalArgumentException("PostgreSQL services are not implemented yet");
            case SQL_SERVER -> throw new IllegalArgumentException("SQL Server services are not implemented yet");
            case ORACLE -> throw new IllegalArgumentException("Oracle Database services are not implemented yet");
            case H2 -> throw new IllegalArgumentException("H2 Database services are not implemented yet");
            case SQLITE -> throw new IllegalArgumentException("SQLite services are not implemented yet");
        };
    }

    @Bean
    @Primary
    public DatabaseInfoService databaseInfoService(MySqlDatabaseInfoManager mySqlDatabaseInfoManager) {
        return switch (databaseType) {
            case MYSQL -> mySqlDatabaseInfoManager;
            case POSTGRESQL -> throw new IllegalArgumentException("PostgreSQL services are not implemented yet");
            case SQL_SERVER -> throw new IllegalArgumentException("SQL Server services are not implemented yet");
            case ORACLE -> throw new IllegalArgumentException("Oracle Database services are not implemented yet");
            case H2 -> throw new IllegalArgumentException("H2 Database services are not implemented yet");
            case SQLITE -> throw new IllegalArgumentException("SQLite services are not implemented yet");
        };
    }
}
