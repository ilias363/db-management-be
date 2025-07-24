package ma.ilias.dbmanagementbe.service;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ma.ilias.dbmanagementbe.enums.ActionType;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
@Slf4j
public class AuditManager implements AuditService {

    private final AuditLogService auditLogService;

    @Override
    public void auditAction(ActionType actionType, String schemaName, String tableName, String objectName,
                            String actionDetails, Boolean successful, String errorMessage) {
        try {
            auditLogService.createAuditLog(actionType, schemaName, tableName, objectName,
                    actionDetails, successful, errorMessage);
        } catch (Exception e) {
            log.error("Failed to create audit log for action: {}", actionType, e);
        }
    }

    @Override
    public void auditSuccessfulAction(ActionType actionType, String schemaName, String tableName, String objectName) {
        auditAction(actionType, schemaName, tableName, objectName, null, true, null);
    }

    @Override
    public void auditSuccessfulAction(ActionType actionType, String schemaName, String tableName) {
        auditAction(actionType, schemaName, tableName, null, null, true, null);
    }

    @Override
    public void auditSuccessfulAction(ActionType actionType, String objectName) {
        auditAction(actionType, null, null, objectName, null, true, null);
    }

    @Override
    public void auditFailedAction(ActionType actionType, String schemaName, String tableName, String objectName, String errorMessage) {
        auditAction(actionType, schemaName, tableName, objectName, null, false, errorMessage);
    }

    @Override
    public void auditFailedAction(ActionType actionType, String schemaName, String tableName, String errorMessage) {
        auditAction(actionType, schemaName, tableName, null, null, false, errorMessage);
    }

    @Override
    public void auditFailedAction(ActionType actionType, String objectName, String errorMessage) {
        auditAction(actionType, null, null, objectName, null, false, errorMessage);
    }
}
