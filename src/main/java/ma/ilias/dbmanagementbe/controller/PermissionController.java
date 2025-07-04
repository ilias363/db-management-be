package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.dto.permission.NewPermissionDto;
import ma.ilias.dbmanagementbe.dto.permission.PermissionDto;
import ma.ilias.dbmanagementbe.dto.permission.UpdatePermissionDto;
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

    @PostMapping
    public ResponseEntity<ApiResponse<PermissionDto>> createPermission(@Valid @RequestBody NewPermissionDto newPermissionDto) {
        PermissionDto savedPermission = permissionService.save(newPermissionDto);
        return new ResponseEntity<>(new ApiResponse<>(
                "Permission created successfully",
                true,
                savedPermission
        ), HttpStatus.CREATED);
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
        PermissionDto updatedPermission = permissionService.update(id, updatePermissionDto);
        return ResponseEntity.ok(new ApiResponse<>(
                "Permission updated successfully",
                true,
                updatedPermission
        ));
    }
}

