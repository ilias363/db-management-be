package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Pattern;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.dto.appuser.*;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.service.AppUserService;
import ma.ilias.dbmanagementbe.service.AuditService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/users")
@AllArgsConstructor
public class AppUserController {

    private final AppUserService appUserService;
    private final AuditService auditService;

    @PostMapping
    public ResponseEntity<ApiResponse<AppUserDto>> createUser(@Valid @RequestBody NewAppUserDto newAppUserDto) {
        try {
            AppUserDto createdUser = appUserService.save(newAppUserDto);

            auditService.auditSuccessfulAction(ActionType.CREATE_USER,
                    newAppUserDto.getUsername() + " (ID: " + newAppUserDto.getId() + ")");

            return new ResponseEntity<>(ApiResponse.<AppUserDto>builder()
                    .message("User created successfully")
                    .success(true)
                    .data(createdUser)
                    .build(), HttpStatus.CREATED);
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.CREATE_USER, newAppUserDto.getUsername(), e.getMessage());
            throw e;
        }
    }

    @GetMapping("/{id:\\d+}")
    public ResponseEntity<ApiResponse<AppUserDto>> getUserById(@PathVariable Long id) {
        AppUserDto appUserDto = appUserService.findById(id);
        return ResponseEntity.ok(ApiResponse.<AppUserDto>builder()
                .message("User fetched successfully")
                .success(true)
                .data(appUserDto)
                .build());
    }

    @GetMapping("/username/{username}")
    public ResponseEntity<ApiResponse<AppUserDto>> getUserByUsername(@PathVariable String username) {
        AppUserDto appUserDto = appUserService.findByUsername(username, true);
        return ResponseEntity.ok(ApiResponse.<AppUserDto>builder()
                .message("User fetched successfully")
                .success(true)
                .data(appUserDto)
                .build());
    }

    @GetMapping
    public ResponseEntity<ApiResponse<AppUserPageDto>> getAllUsersPaginated(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) @Max(100) int size,
            @RequestParam(required = false) String sortBy,
            @RequestParam(defaultValue = "ASC") @Pattern(regexp = "^(ASC|DESC)$",
                    message = "Sort direction must be either ASC or DESC") String sortDirection,
            @RequestParam(required = false) String search
    ) {
        AppUserPageDto userPage = appUserService.findAllPaginated(page, size, sortBy, sortDirection, search);
        return ResponseEntity.ok(ApiResponse.<AppUserPageDto>builder()
                .message("Users fetched successfully")
                .success(true)
                .data(userPage)
                .build());
    }

    @GetMapping("/active")
    public ResponseEntity<ApiResponse<AppUserPageDto>> getAllActiveUsersPaginated(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) @Max(100) int size,
            @RequestParam(required = false) String sortBy,
            @RequestParam(defaultValue = "ASC") @Pattern(regexp = "^(ASC|DESC)$",
                    message = "Sort direction must be either ASC or DESC") String sortDirection,
            @RequestParam(required = false) String search
    ) {
        AppUserPageDto userPage = appUserService.findAllActivePaginated(page, size, sortBy, sortDirection, search);
        return ResponseEntity.ok(ApiResponse.<AppUserPageDto>builder()
                .message("Active users fetched successfully")
                .success(true)
                .data(userPage)
                .build());
    }

    @PutMapping("/{id:\\d+}")
    public ResponseEntity<ApiResponse<AppUserDto>> updateUser(
            @PathVariable Long id,
            @Valid @RequestBody UpdateAppUserDto updateAppUserDto
    ) {
        try {
            AppUserDto currentUser = appUserService.findById(id);
            AppUserDto updatedUser = appUserService.update(id, updateAppUserDto);

            auditService.auditSuccessfulAction(ActionType.UPDATE_USER,
                    currentUser.getUsername() + " (ID: " + id + ")");

            return ResponseEntity.ok(ApiResponse.<AppUserDto>builder()
                    .message("User updated successfully")
                    .success(true)
                    .data(updatedUser)
                    .build());
        } catch (Exception e) {
            try {
                AppUserDto currentUser = appUserService.findById(id);
                auditService.auditFailedAction(ActionType.UPDATE_USER,
                        currentUser.getUsername() + " (ID: " + id + ")",
                        e.getMessage());
            } catch (Exception ignored) {
                auditService.auditFailedAction(ActionType.UPDATE_USER, "Unknown user (ID: " + id + ")", e.getMessage());
            }
            throw e;
        }
    }

//    @DeleteMapping("/{id}")
//    public ResponseEntity<ApiResponse<Void>> deleteUser(@PathVariable Long id) {
//        return appUserService.deleteById(id) ?
//                ResponseEntity.ok(ApiResponse.<Void>builder()
//                        .message("User deleted successfully")
//                        .success(true)
//                        .build())
//                :
//                ResponseEntity.ok(ApiResponse.<Void>builder()
//                        .message("User has not been deleted")
//                        .success(false)
//                        .build());
//    }

    @PutMapping("/{id:\\d+}/deactivate")
    public ResponseEntity<ApiResponse<Void>> deactivateUser(@PathVariable Long id) {
        try {
            AppUserDto user = appUserService.findById(id);
            appUserService.deactivateById(id);

            auditService.auditSuccessfulAction(ActionType.DEACTIVATE_USER, user.getUsername() + " (ID: " + id + ")");

            return ResponseEntity.ok(ApiResponse.<Void>builder()
                    .message("User deactivated successfully")
                    .success(true)
                    .build());
        } catch (Exception e) {
            try {
                AppUserDto user = appUserService.findById(id);
                auditService.auditFailedAction(ActionType.DEACTIVATE_USER, user.getUsername() + " (ID: " + id + ")", e.getMessage());
            } catch (Exception ignored) {
                auditService.auditFailedAction(ActionType.DEACTIVATE_USER, "Unknown user (ID: " + id + ")", e.getMessage());
            }
            throw e;
        }
    }

    @PutMapping("/{id:\\d+}/activate")
    public ResponseEntity<ApiResponse<Void>> activateUser(@PathVariable Long id) {
        try {
            AppUserDto user = appUserService.findById(id);
            appUserService.activateById(id);

            auditService.auditSuccessfulAction(ActionType.ACTIVATE_USER, user.getUsername() + " (ID: " + id + ")");

            return ResponseEntity.ok(ApiResponse.<Void>builder()
                    .message("User activated successfully")
                    .success(true)
                    .build());
        } catch (Exception e) {
            try {
                AppUserDto user = appUserService.findById(id);
                auditService.auditFailedAction(ActionType.ACTIVATE_USER, user.getUsername() + " (ID: " + id + ")", e.getMessage());
            } catch (Exception ignored) {
                auditService.auditFailedAction(ActionType.ACTIVATE_USER, "Unknown user (ID: " + id + ")", e.getMessage());
            }
            throw e;
        }
    }

    @GetMapping("/stats")
    public ResponseEntity<ApiResponse<AppUserStatsDto>> getUserStats() {
        AppUserStatsDto stats = appUserService.getUserStats();
        return ResponseEntity.ok(ApiResponse.<AppUserStatsDto>builder()
                .message("User statistics retrieved successfully")
                .success(true)
                .data(stats)
                .build());
    }
}
