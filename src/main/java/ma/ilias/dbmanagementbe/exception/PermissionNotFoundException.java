package ma.ilias.dbmanagementbe.exception;

public class PermissionNotFoundException extends EntityNotFoundException {
    public PermissionNotFoundException(String message) {
        super(message);
    }
}
