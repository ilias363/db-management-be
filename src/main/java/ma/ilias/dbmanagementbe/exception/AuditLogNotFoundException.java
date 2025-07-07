package ma.ilias.dbmanagementbe.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.NOT_FOUND)
public class AuditLogNotFoundException extends EntityNotFoundException {
    public AuditLogNotFoundException(String message) {
        super(message);
    }
}
