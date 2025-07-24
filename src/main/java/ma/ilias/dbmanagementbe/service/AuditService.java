package ma.ilias.dbmanagementbe.service;

import ma.ilias.dbmanagementbe.enums.ActionType;

public interface AuditService {

    void auditAction(ActionType actionType, String schemaName, String tableName, String objectName,
                     String actionDetails, Boolean successful, String errorMessage);

    void auditSuccessfulAction(ActionType actionType, String schemaName, String tableName, String objectName);

    void auditSuccessfulAction(ActionType actionType, String schemaName, String tableName);

    void auditSuccessfulAction(ActionType actionType, String objectName);

    void auditFailedAction(ActionType actionType, String schemaName, String tableName, String objectName, String errorMessage);

    void auditFailedAction(ActionType actionType, String schemaName, String tableName, String errorMessage);

    void auditFailedAction(ActionType actionType, String objectName, String errorMessage);
}
