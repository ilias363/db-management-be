package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.dto.permission.NewPermissionDto;
import ma.ilias.dbmanagementbe.dto.permission.PermissionDto;
import ma.ilias.dbmanagementbe.dto.permission.UpdatePermissionDto;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.service.AuditService;
import ma.ilias.dbmanagementbe.service.PermissionService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/permissions")
@AllArgsConstructor
public class PermissionController {

    private final PermissionService permissionService;
    private final AuditService auditService;

    @PostMapping
    public ResponseEntity<ApiResponse<PermissionDto>> createPermission(@Valid @RequestBody NewPermissionDto newPermissionDto) {
        try {
            PermissionDto savedPermission = permissionService.save(newPermissionDto);

            auditService.auditSuccessfulAction(ActionType.CREATE_PERMISSION,
                    savedPermission.getPermissionType() + " (ID: " + savedPermission.getId() + ")");

            return new ResponseEntity<>(new ApiResponse<>(
                    "Permission created successfully",
                    true,
                    savedPermission
            ), HttpStatus.CREATED);
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.CREATE_PERMISSION, newPermissionDto.getPermissionType(), e.getMessage());
            throw e;
        }
    }

    @GetMapping
    public ResponseEntity<ApiResponse<List<PermissionDto>>> getAllPermissions() {
        List<PermissionDto> permissions = permissionService.findAll();
        return ResponseEntity.ok(new ApiResponse<>(
                "Permissions retrieved successfully",
                true,
                permissions
        ));
    }

    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<PermissionDto>> getPermissionById(@PathVariable Long id) {
        PermissionDto permission = permissionService.findById(id);
        return ResponseEntity.ok(new ApiResponse<>(
                "Permission retrieved successfully",
                true,
                permission
        ));
    }

    @GetMapping("/type/{permissionType}")
    public ResponseEntity<ApiResponse<List<PermissionDto>>> getPermissionByType(@PathVariable String permissionType) {
        List<PermissionDto> permissions = permissionService.findByPermissionType(permissionType);
        return ResponseEntity.ok(new ApiResponse<>(
                "Permission retrieved successfully by type",
                true,
                permissions
        ));
    }

    @PutMapping("/{id}")
    public ResponseEntity<ApiResponse<PermissionDto>> updatePermission(
            @PathVariable Long id,
            @Valid @RequestBody UpdatePermissionDto updatePermissionDto
    ) {
        try {
            PermissionDto currentPermission = permissionService.findById(id);
            PermissionDto updatedPermission = permissionService.update(id, updatePermissionDto);

            auditService.auditSuccessfulAction(ActionType.UPDATE_PERMISSION, currentPermission.getPermissionType() + " (ID: " + id + ")");

            return ResponseEntity.ok(new ApiResponse<>(
                    "Permission updated successfully",
                    true,
                    updatedPermission
            ));
        } catch (Exception e) {
            try {
                PermissionDto currentPermission = permissionService.findById(id);
                auditService.auditFailedAction(ActionType.UPDATE_PERMISSION, currentPermission.getPermissionType() + " (ID: " + id + ")", e.getMessage());
            } catch (Exception ignored) {
                auditService.auditFailedAction(ActionType.UPDATE_PERMISSION, "Unknown permission  (ID: " + id + ")", e.getMessage());
            }
            throw e;
        }
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<ApiResponse<Void>> deletePermission(@PathVariable Long id) {
        try {
            PermissionDto permission = permissionService.findById(id);
            boolean deleted = permissionService.deleteById(id);

            if (deleted) {
                auditService.auditSuccessfulAction(ActionType.DELETE_PERMISSION, permission.getPermissionType() + " (ID: " + id + ")");

                return ResponseEntity.ok(new ApiResponse<>(
                        "Permission deleted successfully",
                        true,
                        null
                ));
            } else {
                auditService.auditFailedAction(ActionType.DELETE_PERMISSION, permission.getPermissionType() + " (ID: " + id + ")", "Permission deletion failed");

                return ResponseEntity.ok(new ApiResponse<>(
                        "Permission has not been deleted",
                        false,
                        null
                ));
            }
        } catch (Exception e) {
            try {
                PermissionDto permission = permissionService.findById(id);
                auditService.auditFailedAction(ActionType.DELETE_PERMISSION, permission.getPermissionType() + " (ID: " + id + ")", e.getMessage());
            } catch (Exception ignored) {
                auditService.auditFailedAction(ActionType.DELETE_PERMISSION, "Unknown permission (ID: " + id + ")", e.getMessage());
            }
            throw e;
        }
    }
}

