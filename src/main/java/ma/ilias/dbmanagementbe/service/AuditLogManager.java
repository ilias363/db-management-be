package ma.ilias.dbmanagementbe.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import ma.ilias.dbmanagementbe.dao.entities.AuditLog;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dao.repositories.AuditLogRepository;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogDto;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.exception.AuditLogNotFoundException;
import ma.ilias.dbmanagementbe.exception.UserNotFoundException;
import ma.ilias.dbmanagementbe.mapper.AuditLogMapper;
import ma.ilias.dbmanagementbe.util.AuditDescriptionBuilder;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
@Transactional
public class AuditLogManager implements AuditLogService {

    private final AuditLogRepository auditLogRepository;
    private final AppUserRepository appUserRepository;
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
        if (!appUserRepository.existsById(userId)) {
            throw new UserNotFoundException("User not found with ID: " + userId);
        }
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

    @Override
    public AuditLogDto createAuditLog(ActionType actionType, String schemaName, String tableName,
                                      String objectName, String actionDetails, Boolean successful, String errorMessage) {
        AppUser currentUser = getCurrentUser();
        if (currentUser == null) {
            throw new RuntimeException("No authenticated user found for audit logging");
        }

        String finalActionDetails = actionDetails;
        if (actionDetails == null || actionDetails.isBlank()) {
            finalActionDetails = AuditDescriptionBuilder.build(actionType, schemaName, tableName, objectName);
        }

        AuditLog auditLog = AuditLog.builder()
                .user(currentUser)
                .actionType(actionType)
                .schemaName(schemaName)
                .tableName(tableName)
                .objectName(objectName)
                .actionDetails(finalActionDetails)
                .successful(successful)
                .errorMessage(errorMessage)
                .build();

        AuditLog savedAuditLog = auditLogRepository.save(auditLog);
        return auditLogMapper.toDto(savedAuditLog);
    }

    private AppUser getCurrentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.isAuthenticated() &&
                !"anonymousUser".equals(authentication.getPrincipal())) {

            if (authentication.getPrincipal() instanceof AppUser) {
                return (AppUser) authentication.getPrincipal();
            } else if (authentication.getName() != null) {
                return appUserRepository.findByUsername(authentication.getName()).orElse(null);
            }
        }
        return null;
    }
}
