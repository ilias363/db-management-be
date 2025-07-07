package ma.ilias.dbmanagementbe.service;

import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogDto;

import java.util.List;
import java.util.Optional;

public interface AuditLogService {
    AuditLogDto findById(Long id);
    List<AuditLogDto> findAll();
    List<AuditLogDto> findByUserId(Long userId);
    Boolean deleteById(Long id);
}
