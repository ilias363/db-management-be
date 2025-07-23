package ma.ilias.dbmanagementbe.exception;

public class RecordNotFoundException extends RuntimeException {
    public RecordNotFoundException(String message) {
        super(message);
    }
    
    public RecordNotFoundException(String tableName, Object primaryKey) {
        super(String.format("Record not found in table '%s' with primary key: %s", tableName, primaryKey));
    }
}
