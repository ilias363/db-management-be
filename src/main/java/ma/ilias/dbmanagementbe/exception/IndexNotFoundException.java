package ma.ilias.dbmanagementbe.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.NOT_FOUND)
public class IndexNotFoundException extends MetadataNotFoundException {
    public IndexNotFoundException(String schemaName, String tableName, String indexName) {
        super("Index not found: " + schemaName + "." + tableName + "." + indexName);
    }
}
