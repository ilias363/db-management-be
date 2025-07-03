package ma.ilias.dbmanagementbe.exception;

public class RoleNotFoundException extends EntityNotFoundException {
    public RoleNotFoundException(String message) {
        super(message);
    }
}
