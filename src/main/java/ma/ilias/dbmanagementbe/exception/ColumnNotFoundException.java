package ma.ilias.dbmanagementbe.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.NOT_FOUND)
public class ColumnNotFoundException extends MetadataNotFoundException {
    public ColumnNotFoundException(String schemaName, String tableName, String columnName) {
        super("Column not found: " + schemaName + "." + tableName + "." + columnName);
    }
}
