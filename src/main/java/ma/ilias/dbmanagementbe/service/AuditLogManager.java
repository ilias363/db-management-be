package ma.ilias.dbmanagementbe.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import ma.ilias.dbmanagementbe.dao.entities.AuditLog;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dao.repositories.AuditLogRepository;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogDto;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogPageDto;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditStatsDto;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.exception.AuditLogNotFoundException;
import ma.ilias.dbmanagementbe.exception.InsufficientPermissionException;
import ma.ilias.dbmanagementbe.exception.UserNotFoundException;
import ma.ilias.dbmanagementbe.mapper.AuditLogMapper;
import ma.ilias.dbmanagementbe.util.AuditDescriptionBuilder;
import ma.ilias.dbmanagementbe.util.AuthorizationUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Set;

@Service
@AllArgsConstructor
@Transactional
public class AuditLogManager implements AuditLogService {

    private final AuditLogRepository auditLogRepository;
    private final AppUserRepository appUserRepository;
    private final AuditLogMapper auditLogMapper;

    @Override
    public AuditLogDto findById(Long id) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can view audit logs");
        }

        AuditLog auditLog = auditLogRepository.findById(id)
                .orElseThrow(() -> new AuditLogNotFoundException("AuditLog not found with ID: " + id));
        return auditLogMapper.toDto(auditLog);
    }

    @Override
    public AuditLogPageDto findAllPaginated(int page, int size, String sortBy, String sortDirection) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can view audit logs");
        }

        Sort sort = createSort(sortBy, sortDirection);
        Pageable pageable = PageRequest.of(page, size, sort);

        Page<AuditLog> auditLogPage = auditLogRepository.findAll(pageable);

        return AuditLogPageDto.builder()
                .items(auditLogPage.getContent().stream()
                        .map(auditLogMapper::toDto)
                        .toList())
                .totalItems(auditLogPage.getTotalElements())
                .currentPage(page)
                .pageSize(size)
                .totalPages(auditLogPage.getTotalPages())
                .build();
    }

    @Override
    public AuditLogPageDto findByUserIdPaginated(Long userId, int page, int size, String sortBy, String sortDirection) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can view audit logs");
        }

        if (!appUserRepository.existsById(userId)) {
            throw new UserNotFoundException("User not found with ID: " + userId);
        }

        Sort sort = createSort(sortBy, sortDirection);
        Pageable pageable = PageRequest.of(page, size, sort);

        Page<AuditLog> auditLogPage = auditLogRepository.findByUser_Id(userId, pageable);

        return AuditLogPageDto.builder()
                .items(auditLogPage.getContent().stream()
                        .map(auditLogMapper::toDto)
                        .toList())
                .totalItems(auditLogPage.getTotalElements())
                .currentPage(page)
                .pageSize(size)
                .totalPages(auditLogPage.getTotalPages())
                .build();
    }

    @Override
    public Boolean deleteById(Long id) {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can delete audit logs");
        }

        if (!auditLogRepository.existsById(id)) {
            throw new AuditLogNotFoundException("AuditLog not found with ID: " + id);
        }
        auditLogRepository.deleteById(id);
        return !auditLogRepository.existsById(id);
    }

    @Override
    public void createAuditLog(ActionType actionType, String schemaName, String tableName,
                               String objectName, String actionDetails, Boolean successful, String errorMessage) {
        AppUser currentUser = getCurrentUser();
        if (currentUser == null && !Set.of(ActionType.LOGIN, ActionType.LOGOUT).contains(actionType)) {
            throw new RuntimeException("No authenticated user found for audit logging");
        }

        String finalActionDetails = actionDetails;
        if (actionDetails == null || actionDetails.isBlank()) {
            if (schemaName != null && tableName != null && objectName != null) {
                finalActionDetails = AuditDescriptionBuilder.build(actionType, schemaName, tableName, objectName);
            } else if (schemaName != null && tableName != null) {
                finalActionDetails = AuditDescriptionBuilder.build(actionType, schemaName, tableName);
            } else if (objectName != null) {
                finalActionDetails = AuditDescriptionBuilder.build(actionType, objectName);
            } else {
                finalActionDetails = AuditDescriptionBuilder.build(actionType, schemaName);
            }
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

        auditLogRepository.save(auditLog);
    }

    @Override
    public AuditStatsDto getAuditStats() {
        if (!AuthorizationUtils.hasUserManagementAccess()) {
            throw new InsufficientPermissionException("Only administrators can view audit statistics");
        }

        long totalAudits = auditLogRepository.count();
        long totalSuccessful = auditLogRepository.countSuccessfulAudits();
        long totalFailed = auditLogRepository.countFailedAudits();
        long last24hActivityCount = auditLogRepository.countAuditsLast24Hours();
        String mostCommonActionString = auditLogRepository.findMostCommonActionString();
        ActionType mostCommonAction = null;
        if (mostCommonActionString != null) {
            try {
                mostCommonAction = ActionType.valueOf(mostCommonActionString);
            } catch (IllegalArgumentException ignored) {
            }
        }
        Double averageActionsPerDay = auditLogRepository.calculateAverageActionsPerDay();

        double failedPercentage = totalAudits > 0 ? (double) totalFailed / totalAudits * 100 : 0.0;

        return AuditStatsDto.builder()
                .totalAudits(totalAudits)
                .totalSuccessful(totalSuccessful)
                .totalFailed(totalFailed)
                .failedPercentage(Math.round(failedPercentage * 100.0) / 100.0)
                .last24hActivityCount(last24hActivityCount)
                .mostCommonAction(mostCommonAction)
                .averageActionsPerDay(averageActionsPerDay != null ? Math.round(averageActionsPerDay * 100.0) / 100.0 : 0.0)
                .build();
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

    private Sort createSort(String sortBy, String sortDirection) {
        final List<String> validFields = List.of(
                "id", "user", "actionType", "schemaName", "tableName",
                "objectName", "actionDetails", "successful", "errorMessage", "auditTimestamp"
        );

        if (sortBy == null || sortBy.isBlank() || !validFields.contains(sortBy)) {
            sortBy = "auditTimestamp";
        }

        Sort.Direction direction = Sort.Direction.DESC;
        if ("ASC".equalsIgnoreCase(sortDirection)) {
            direction = Sort.Direction.ASC;
        }

        return Sort.by(direction, sortBy);
    }
}
