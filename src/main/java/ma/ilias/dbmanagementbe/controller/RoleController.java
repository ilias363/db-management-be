package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.dto.role.NewRoleDto;
import ma.ilias.dbmanagementbe.dto.role.RoleDto;
import ma.ilias.dbmanagementbe.dto.role.UpdateRoleDto;
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

    @PostMapping
    public ResponseEntity<ApiResponse<RoleDto>> createRole(@Valid @RequestBody NewRoleDto newRoleDto) {
        RoleDto createdRole = roleService.save(newRoleDto);
        return new ResponseEntity<>(ApiResponse.<RoleDto>builder()
                .message("Role created successfully")
                .success(true)
                .data(createdRole)
                .build(), HttpStatus.CREATED);
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
        RoleDto updatedRole = roleService.update(id, updateRoleDto);
        return ResponseEntity.ok(ApiResponse.<RoleDto>builder()
                .message("Role updated successfully")
                .success(true)
                .data(updatedRole)
                .build());
    }
}
