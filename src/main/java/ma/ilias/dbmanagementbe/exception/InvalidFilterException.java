package ma.ilias.dbmanagementbe.exception;

/**
 * Exception thrown when filter validation fails due to incompatible data types or operators.
 */
public class InvalidFilterException extends RuntimeException {
    
    public InvalidFilterException(String message) {
        super(message);
    }
    
    public InvalidFilterException(String message, Throwable cause) {
        super(message, cause);
    }
    
    public InvalidFilterException(String columnName, String operator, String dataType, String reason) {
        super(String.format("Invalid filter for column '%s': operator '%s' cannot be used with data type '%s'. %s", 
                           columnName, operator, dataType, reason));
    }
    
    public InvalidFilterException(String columnName, String value, String dataType, String expectedFormat) {
        super(String.format("Invalid value '%s' for column '%s' of type '%s'. Expected format: %s", 
                           value, columnName, dataType, expectedFormat));
    }
}
