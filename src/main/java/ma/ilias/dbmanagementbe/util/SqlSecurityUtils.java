package ma.ilias.dbmanagementbe.util;

import java.util.Set;
import java.util.regex.Pattern;

public class SqlSecurityUtils {

    // Pattern for valid SQL identifiers (schema names, table names, column names, index names)
    private static final Pattern VALID_IDENTIFIER = Pattern.compile("^[a-zA-Z_][a-zA-Z0-9_]*$");

    // Maximum length for identifiers to prevent buffer overflow
    private static final int MAX_IDENTIFIER_LENGTH = 64;

    // Reserved MySQL keywords that should not be used as identifiers
// Using HashSet for O(1) lookup instead of O(n) array search
    private static final Set<String> MYSQL_RESERVED_KEYWORDS = Set.of(
            "SELECT", "INSERT", "UPDATE", "DELETE", "CREATE", "DROP", "ALTER", "TABLE",
            "DATABASE", "SCHEMA", "INDEX", "PRIMARY", "FOREIGN", "KEY", "CONSTRAINT",
            "FROM", "WHERE", "GROUP", "ORDER", "BY", "HAVING", "UNION", "JOIN",
            "INNER", "LEFT", "RIGHT", "OUTER", "ON", "AS", "DISTINCT", "ALL",
            "AND", "OR", "NOT", "NULL", "TRUE", "FALSE", "IS", "IN", "EXISTS",
            "BETWEEN", "LIKE", "REGEXP", "RLIKE", "CASE", "WHEN", "THEN", "ELSE", "END"
    );

    /**
     * Validates that an identifier (schema name, table name, column name) is safe to use in SQL.
     * Prevents SQL injection by ensuring the identifier contains only allowed characters.
     *
     * @param identifier     the identifier to validate
     * @param identifierType the type of identifier (for error messages)
     * @return the validated identifier
     * @throws IllegalArgumentException if the identifier is invalid or potentially dangerous
     */
    public static String validateIdentifier(String identifier, String identifierType) {
        if (identifier == null || identifier.isBlank()) {
            throw new IllegalArgumentException(identifierType + " cannot be null or empty");
        }

        String trimmed = identifier.trim();

        if (trimmed.length() > MAX_IDENTIFIER_LENGTH) {
            throw new IllegalArgumentException(identifierType + " is too long (max " + MAX_IDENTIFIER_LENGTH + " characters)");
        }

        if (MYSQL_RESERVED_KEYWORDS.contains(trimmed.toUpperCase())) {
            throw new IllegalArgumentException(identifierType + " cannot be a reserved MySQL keyword: " + trimmed);
        }

        if (!VALID_IDENTIFIER.matcher(trimmed).matches()) {
            throw new IllegalArgumentException(identifierType +
                    " contains invalid characters. Only letters, numbers, and underscores are allowed," +
                    " and it must start with a letter or underscore.");
        }

        return trimmed;
    }

    /**
     * Validates a schema name for SQL safety.
     *
     * @param schemaName the schema name to validate
     * @return the validated schema name
     * @throws IllegalArgumentException if the schema name is invalid
     */
    public static String validateSchemaName(String schemaName) {
        return validateIdentifier(schemaName, "Schema name");
    }

    /**
     * Validates a table name for SQL safety.
     *
     * @param tableName the table name to validate
     * @return the validated table name
     * @throws IllegalArgumentException if the table name is invalid
     */
    public static String validateTableName(String tableName) {
        return validateIdentifier(tableName, "Table name");
    }

    /**
     * Validates a column name for SQL safety.
     *
     * @param columnName the column name to validate
     * @return the validated column name
     * @throws IllegalArgumentException if the column name is invalid
     */
    public static String validateColumnName(String columnName) {
        return validateIdentifier(columnName, "Column name");
    }

    /**
     * Validates an index name for SQL safety.
     *
     * @param indexName the index name to validate
     * @return the validated index name
     * @throws IllegalArgumentException if the index name is invalid
     */
    public static String validateIndexName(String indexName) {
        return validateIdentifier(indexName, "Index name");
    }

    /**
     * Sanitizes a default value for use in columns.
     *
     * @param defaultValue the default value to sanitize
     * @return the sanitized default value
     */
    public static String sanitizeDefaultValue(String defaultValue) {
        if (defaultValue == null) {
            return "NULL";
        }
        // Escape single quotes to prevent SQL injection
        return "'" + defaultValue.replace("'", "''") + "'";
    }
}
