package ma.ilias.dbmanagementbe.service;

import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogDto;
import ma.ilias.dbmanagementbe.enums.ActionType;

import java.util.List;

public interface AuditLogService {
    AuditLogDto findById(Long id);

    List<AuditLogDto> findAll();

    List<AuditLogDto> findByUserId(Long userId);

    Boolean deleteById(Long id);

    void createAuditLog(ActionType actionType, String schemaName, String tableName,
                        String objectName, String actionDetails, Boolean successful, String errorMessage);
}
