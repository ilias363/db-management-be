package ma.ilias.dbmanagementbe.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.AuditLog;
import ma.ilias.dbmanagementbe.dao.repositories.AuditLogRepository;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogDto;
import ma.ilias.dbmanagementbe.exception.AuditLogNotFoundException;
import ma.ilias.dbmanagementbe.mapper.AuditLogMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
@Transactional
public class AuditLogManager implements AuditLogService {

    private final AuditLogRepository auditLogRepository;
    private final AuditLogMapper auditLogMapper;

    @Override
    public AuditLogDto findById(Long id) {
        AuditLog auditLog = auditLogRepository.findById(id)
                .orElseThrow(() -> new AuditLogNotFoundException("AuditLog not found with ID: " + id));
        return auditLogMapper.toDto(auditLog);
    }

    @Override
    public List<AuditLogDto> findAll() {
        return auditLogRepository.findAll().stream()
                .map(auditLogMapper::toDto)
                .collect(Collectors.toList());
    }

    @Override
    public List<AuditLogDto> findByUserId(Long userId) {
        return auditLogRepository.findByUser_Id(userId).stream()
                .map(auditLogMapper::toDto)
                .collect(Collectors.toList());
    }

    @Override
    public Boolean deleteById(Long id) {
        if (!auditLogRepository.existsById(id)) {
            throw new AuditLogNotFoundException("AuditLog not found with ID: " + id);
        }
        auditLogRepository.deleteById(id);
        return !auditLogRepository.existsById(id);
    }
}
