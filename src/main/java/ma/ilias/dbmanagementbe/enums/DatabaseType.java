package ma.ilias.dbmanagementbe.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum DatabaseType {
    MYSQL("MySQL", "mysql"),
    POSTGRESQL("PostgreSQL", "postgresql"),
    SQL_SERVER("Microsoft SQL Server", "sqlserver"),
    ORACLE("Oracle Database", "oracle"),
    H2("H2 Database", "h2"),
    SQLITE("SQLite", "sqlite");

    private final String displayName;
    private final String urlIdentifier;

    public static DatabaseType fromJdbcUrl(String jdbcUrl) {
        if (jdbcUrl == null || !jdbcUrl.startsWith("jdbc:")) {
            throw new IllegalArgumentException("Invalid JDBC URL: " + jdbcUrl);
        }

        String lowerUrl = jdbcUrl.toLowerCase();

        for (DatabaseType type : values()) {
            if (lowerUrl.contains(":" + type.urlIdentifier + ":")) {
                return type;
            }
        }

        throw new IllegalArgumentException("Unsupported database type in JDBC URL: " + jdbcUrl);
    }
}
