package ma.ilias.dbmanagementbe.controller;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogDto;
import ma.ilias.dbmanagementbe.service.AuditLogService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/audit-logs")
@AllArgsConstructor
public class AuditLogController {

    private final AuditLogService auditLogService;

    @GetMapping
    public ResponseEntity<ApiResponse<List<AuditLogDto>>> getAllAuditLogs() {
        List<AuditLogDto> auditLogs = auditLogService.findAll();
        return ResponseEntity.ok(ApiResponse.<List<AuditLogDto>>builder()
                .message("Audit logs fetched successfully")
                .success(true)
                .data(auditLogs)
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
    public ResponseEntity<ApiResponse<List<AuditLogDto>>> getAuditLogsByUserId(@PathVariable Long userId) {
        List<AuditLogDto> auditLogs = auditLogService.findByUserId(userId);
        return ResponseEntity.ok(ApiResponse.<List<AuditLogDto>>builder()
                .message("Audit logs for user fetched successfully")
                .success(true)
                .data(auditLogs)
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
