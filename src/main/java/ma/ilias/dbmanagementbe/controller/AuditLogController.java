package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Pattern;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogDto;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogPageDto;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogStatsDto;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.service.AuditLogService;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;

@RestController
@RequestMapping("/api/audit-logs")
@AllArgsConstructor
public class AuditLogController {

    private final AuditLogService auditLogService;

    @GetMapping
    public ResponseEntity<ApiResponse<AuditLogPageDto>> getAllAuditLogsPaginated(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) @Max(100) int size,
            @RequestParam(required = false) String sortBy,
            @RequestParam(defaultValue = "DESC") @Pattern(regexp = "^(ASC|DESC)$",
                    message = "Sort direction must be either ASC or DESC") String sortDirection,
            @RequestParam(required = false) String search,
            @RequestParam(required = false) Long userId,
            @RequestParam(required = false) String actionType,
            @RequestParam(required = false) Boolean successful,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime after,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime before
    ) {
        ActionType actionTypeEnum = null;
        if (actionType != null && !actionType.isBlank()) {
            try {
                actionTypeEnum = ActionType.valueOf(actionType.toUpperCase());
            } catch (IllegalArgumentException e) {
                return ResponseEntity.badRequest().body(ApiResponse.<AuditLogPageDto>builder()
                        .message("Invalid action type: " + actionType)
                        .success(false)
                        .build());
            }
        }

        AuditLogPageDto auditLogPage = auditLogService.findAllPaginated(page, size, sortBy, sortDirection,
                search, userId, actionTypeEnum, successful, after, before);
        return ResponseEntity.ok(ApiResponse.<AuditLogPageDto>builder()
                .message("Audit logs fetched successfully")
                .success(true)
                .data(auditLogPage)
                .build());
    }

    @GetMapping("/{id:\\d+}")
    public ResponseEntity<ApiResponse<AuditLogDto>> getAuditLogById(@PathVariable Long id) {
        AuditLogDto auditLog = auditLogService.findById(id);
        return ResponseEntity.ok(ApiResponse.<AuditLogDto>builder()
                .message("AuditLog fetched successfully")
                .success(true)
                .data(auditLog)
                .build());
    }

    @GetMapping("/user/{userId:\\d+}")
    public ResponseEntity<ApiResponse<AuditLogPageDto>> getAuditLogsByUserIdPaginated(
            @PathVariable Long userId,
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) @Max(100) int size,
            @RequestParam(required = false) String sortBy,
            @RequestParam(defaultValue = "DESC") @Pattern(regexp = "^(ASC|DESC)$",
                    message = "Sort direction must be either ASC or DESC") String sortDirection
    ) {
        AuditLogPageDto auditLogPage = auditLogService.findByUserIdPaginated(userId, page, size, sortBy, sortDirection);
        return ResponseEntity.ok(ApiResponse.<AuditLogPageDto>builder()
                .message("Audit logs for user fetched successfully")
                .success(true)
                .data(auditLogPage)
                .build());
    }

    @DeleteMapping("/{id:\\d+}")
    public ResponseEntity<ApiResponse<Void>> deleteAuditLog(@PathVariable Long id) {
        return auditLogService.deleteById(id) ?
                ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("AuditLog deleted successfully")
                        .success(true)
                        .build())
                :
                ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("AuditLog has not been deleted")
                        .success(false)
                        .build());
    }

    @GetMapping("/stats")
    public ResponseEntity<ApiResponse<AuditLogStatsDto>> getAuditStats() {
        AuditLogStatsDto stats = auditLogService.getAuditStats();
        return ResponseEntity.ok(ApiResponse.<AuditLogStatsDto>builder()
                .message("Audit statistics retrieved successfully")
                .success(true)
                .data(stats)
                .build());
    }
}
