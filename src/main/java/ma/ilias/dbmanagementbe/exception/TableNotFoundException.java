package ma.ilias.dbmanagementbe.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.NOT_FOUND)
public class TableNotFoundException extends MetadataNotFoundException {
    public TableNotFoundException(String schemaName, String tableName) {
        super("Table not found: " + schemaName + "." + tableName);
    }
}
