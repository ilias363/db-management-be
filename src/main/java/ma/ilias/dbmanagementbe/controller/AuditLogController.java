package ma.ilias.dbmanagementbe.controller;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogDto;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogPageDto;
import ma.ilias.dbmanagementbe.service.AuditLogService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/audit-logs")
@AllArgsConstructor
public class AuditLogController {

    private final AuditLogService auditLogService;

    @GetMapping
    public ResponseEntity<ApiResponse<AuditLogPageDto>> getAllAuditLogsPaginated(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(required = false) String sortBy,
            @RequestParam(defaultValue = "DESC") String sortDirection
    ) {
        AuditLogPageDto auditLogPage = auditLogService.findAllPaginated(page, size, sortBy, sortDirection);
        return ResponseEntity.ok(ApiResponse.<AuditLogPageDto>builder()
                .message("Audit logs fetched successfully")
                .success(true)
                .data(auditLogPage)
                .build());
    }

    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<AuditLogDto>> getAuditLogById(@PathVariable Long id) {
        AuditLogDto auditLog = auditLogService.findById(id);
        return ResponseEntity.ok(ApiResponse.<AuditLogDto>builder()
                .message("AuditLog fetched successfully")
                .success(true)
                .data(auditLog)
                .build());
    }

    @GetMapping("/user/{userId}")
    public ResponseEntity<ApiResponse<AuditLogPageDto>> getAuditLogsByUserIdPaginated(
            @PathVariable Long userId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(required = false) String sortBy,
            @RequestParam(defaultValue = "DESC") String sortDirection
    ) {
        AuditLogPageDto auditLogPage = auditLogService.findByUserIdPaginated(userId, page, size, sortBy, sortDirection);
        return ResponseEntity.ok(ApiResponse.<AuditLogPageDto>builder()
                .message("Audit logs for user fetched successfully")
                .success(true)
                .data(auditLogPage)
                .build());
    }

    @DeleteMapping("/{id}")
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
}
