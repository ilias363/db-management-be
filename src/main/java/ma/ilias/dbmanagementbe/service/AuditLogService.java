package ma.ilias.dbmanagementbe.service;

import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogDto;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogPageDto;
import ma.ilias.dbmanagementbe.enums.ActionType;

public interface AuditLogService {
    AuditLogDto findById(Long id);

    AuditLogPageDto findAllPaginated(int page, int size, String sortBy, String sortDirection);

    AuditLogPageDto findByUserIdPaginated(Long userId, int page, int size, String sortBy, String sortDirection);

    Boolean deleteById(Long id);

    void createAuditLog(ActionType actionType, String schemaName, String tableName,
                        String objectName, String actionDetails, Boolean successful, String errorMessage);
}
