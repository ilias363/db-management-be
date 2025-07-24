package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.dto.role.NewRoleDto;
import ma.ilias.dbmanagementbe.dto.role.RoleDto;
import ma.ilias.dbmanagementbe.dto.role.UpdateRoleDto;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.service.AuditService;
import ma.ilias.dbmanagementbe.service.RoleService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/roles")
@AllArgsConstructor
public class RoleController {

    private final RoleService roleService;
    private final AuditService auditService;

    @PostMapping
    public ResponseEntity<ApiResponse<RoleDto>> createRole(@Valid @RequestBody NewRoleDto newRoleDto) {
        try {
            RoleDto createdRole = roleService.save(newRoleDto);

            auditService.auditSuccessfulAction(ActionType.CREATE_ROLE, newRoleDto.getName() + " (ID: " + createdRole.getId() + ")");

            return new ResponseEntity<>(ApiResponse.<RoleDto>builder()
                    .message("Role created successfully")
                    .success(true)
                    .data(createdRole)
                    .build(), HttpStatus.CREATED);
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.CREATE_ROLE, newRoleDto.getName(), e.getMessage());
            throw e;
        }
    }

    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<RoleDto>> getRoleById(@PathVariable Long id) {
        RoleDto roleDto = roleService.findById(id);
        return ResponseEntity.ok(ApiResponse.<RoleDto>builder()
                .message("Role fetched successfully")
                .success(true)
                .data(roleDto)
                .build());
    }

    @GetMapping
    public ResponseEntity<ApiResponse<List<RoleDto>>> getAllRoles() {
        List<RoleDto> roles = roleService.findAll();
        return ResponseEntity.ok(ApiResponse.<List<RoleDto>>builder()
                .message("Roles fetched successfully")
                .success(true)
                .data(roles)
                .build());
    }

    @PutMapping("/{id}")
    public ResponseEntity<ApiResponse<RoleDto>> updateRole(
            @PathVariable Long id,
            @Valid @RequestBody UpdateRoleDto updateRoleDto
    ) {
        try {
            RoleDto currentRole = roleService.findById(id);
            RoleDto updatedRole = roleService.update(id, updateRoleDto);

            auditService.auditSuccessfulAction(ActionType.UPDATE_ROLE, currentRole.getName() + " (ID: " + id + ")");

            return ResponseEntity.ok(ApiResponse.<RoleDto>builder()
                    .message("Role updated successfully")
                    .success(true)
                    .data(updatedRole)
                    .build());
        } catch (Exception e) {
            try {
                RoleDto currentRole = roleService.findById(id);
                auditService.auditFailedAction(ActionType.UPDATE_ROLE, currentRole.getName() + " (ID: " + id + ")", e.getMessage());
            } catch (Exception ignored) {
                auditService.auditFailedAction(ActionType.UPDATE_ROLE, "Unknown role (ID: " + id + ")", e.getMessage());
            }
            throw e;
        }
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<ApiResponse<Void>> deleteRole(@PathVariable Long id) {
        try {
            RoleDto role = roleService.findById(id);
            boolean deleted = roleService.deleteById(id);

            if (deleted) {
                auditService.auditSuccessfulAction(ActionType.DELETE_ROLE, role.getName() + " (ID: " + id + ")");

                return ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Role deleted successfully")
                        .success(true)
                        .build());
            } else {
                auditService.auditFailedAction(ActionType.DELETE_ROLE, role.getName() + " (ID: " + id + ")", "Role deletion failed");

                return ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Role has not been deleted")
                        .success(false)
                        .build());
            }
        } catch (Exception e) {
            try {
                RoleDto role = roleService.findById(id);
                auditService.auditFailedAction(ActionType.DELETE_ROLE, role.getName() + " (ID: " + id + ")", e.getMessage());
            } catch (Exception ignored) {
                auditService.auditFailedAction(ActionType.DELETE_ROLE, "Unknown role (ID: " + id + ")", e.getMessage());
            }
            throw e;
        }
    }
}
