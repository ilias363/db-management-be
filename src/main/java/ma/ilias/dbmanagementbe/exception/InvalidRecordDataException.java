package ma.ilias.dbmanagementbe.exception;

public class InvalidRecordDataException extends RuntimeException {
    public InvalidRecordDataException(String message) {
        super(message);
    }
    
    public InvalidRecordDataException(String tableName, String reason) {
        super(String.format("Invalid record data for table '%s': %s", tableName, reason));
    }
}
