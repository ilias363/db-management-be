package ma.ilias.dbmanagementbe.controller;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.dto.appuser.AppUserDto;
import ma.ilias.dbmanagementbe.dto.auth.LoginRequestDto;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.enums.PermissionType;
import ma.ilias.dbmanagementbe.service.AppUserService;
import ma.ilias.dbmanagementbe.service.AuditService;
import ma.ilias.dbmanagementbe.util.AuthorizationUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

@AllArgsConstructor
@RestController
@RequestMapping("api/auth")
public class AuthController {

    private final AuthenticationManager authenticationManager;
    private final AuditService auditService;
    private final AppUserService appUserService;

    @PostMapping("/login")
    public ResponseEntity<ApiResponse<Void>> login(@Valid @RequestBody LoginRequestDto loginRequestDto,
                                                   HttpServletRequest request) {
        try {
            Authentication authentication = authenticationManager.authenticate(
                    new UsernamePasswordAuthenticationToken(
                            loginRequestDto.getUsername(),
                            loginRequestDto.getPassword()
                    )
            );

            SecurityContextHolder.getContext().setAuthentication(authentication);

            HttpSession session = request.getSession(false);
            if (session != null) {
                session.invalidate();
            }
            HttpSession newSession = request.getSession(true);
            newSession.setAttribute("SPRING_SECURITY_CONTEXT", SecurityContextHolder.getContext());

            AppUserDto user = appUserService.findByUsername(loginRequestDto.getUsername());

            auditService.auditSuccessfulAction(ActionType.LOGIN, user.getUsername() + " (ID: " + user.getId() + ")");

            return ResponseEntity.ok(ApiResponse.<Void>builder()
                    .message("Login successful")
                    .success(true)
                    .build());
        } catch (BadCredentialsException ex) {
            AppUserDto user = appUserService.findByUsername(loginRequestDto.getUsername());
            auditService.auditFailedAction(ActionType.LOGIN,
                    user.getUsername() + " (ID: " + user.getId() + ")",
                    "Invalid credentials");

            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(
                    ApiResponse.<Void>builder()
                            .message("Invalid credentials")
                            .success(false)
                            .build()
            );
        } catch (Exception ex) {
            AppUserDto user = appUserService.findByUsername(loginRequestDto.getUsername());
            auditService.auditFailedAction(ActionType.LOGIN,
                    user.getUsername() + " (ID: " + user.getId() + ")",
                    "Authentication failed: " + ex.getMessage());

            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(
                    ApiResponse.<Void>builder()
                            .message("Authentication failed : " + ex.getMessage())
                            .success(false)
                            .build()
            );
        }
    }

    @PostMapping("/logout")
    public ResponseEntity<ApiResponse<Void>> logout(HttpServletRequest request) {
        // Get current user before clearing context for audit
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        String username = (auth != null && auth.isAuthenticated()) ? auth.getName() : "unknown";

        HttpSession session = request.getSession(false);
        if (session != null) {
            session.invalidate();
        }

        SecurityContextHolder.clearContext();

        if (!username.equals("unknown")) {
            AppUserDto user = appUserService.findByUsername(username);
            auditService.auditSuccessfulAction(ActionType.LOGOUT, username + " (ID: " + user.getId() + ")");
        } else {
            auditService.auditSuccessfulAction(ActionType.LOGOUT, username);
        }


        return ResponseEntity.ok(ApiResponse.<Void>builder()
                .message("Logout successful")
                .success(true)
                .build());
    }

    @GetMapping("/isloggedin")
    public ResponseEntity<ApiResponse<Map<String, Boolean>>> isLoggedIn(HttpServletRequest request) {
        HttpSession session = request.getSession(false);
        boolean isLoggedIn = session != null && session.getAttribute("SPRING_SECURITY_CONTEXT") != null;

        return ResponseEntity.ok(
                ApiResponse.<Map<String, Boolean>>builder()
                        .message("Successful")
                        .success(true)
                        .data(Map.of("isLoggedIn", isLoggedIn))
                        .build()
        );
    }

    @GetMapping("/current-user")
    public ResponseEntity<ApiResponse<AppUserDto>> getCurrentUser() {
        AppUserDto appUser = appUserService.getCurrentUserInfo();

        return ResponseEntity.ok(
                ApiResponse.<AppUserDto>builder()
                        .message(appUser != null ? "Successful" : "Failed")
                        .success(appUser != null)
                        .data(appUser)
                        .build()
        );
    }

    @GetMapping("current-user/permissions")
    public ResponseEntity<ApiResponse<Map<String, Boolean>>> getCurrentUserPermissions() {
        Map<String, Boolean> permissions = new HashMap<>();

        permissions.put("isAdmin", AuthorizationUtils.isAdmin());
        permissions.put("isViewer", AuthorizationUtils.isViewer());
        permissions.put("hasUserManagementAccess", AuthorizationUtils.hasUserManagementAccess());
        permissions.put("hasDbAccess", AuthorizationUtils.hasDbAccess());
        permissions.put("hasDbReadAccess", AuthorizationUtils.hasDbReadAccess());
        permissions.put("hasDbWriteAccess", AuthorizationUtils.hasDbWriteAccess());

        return ResponseEntity.ok(ApiResponse.<Map<String, Boolean>>builder()
                .message("User permissions retrieved successfully")
                .success(true)
                .data(permissions)
                .build());
    }

    @GetMapping("current-user/detailed-permissions")
    public ResponseEntity<ApiResponse<Map<String, Object>>> getDetailedPermissions(
            @RequestParam(required = false) String schemaName,
            @RequestParam(required = false) String tableName) {

        Map<String, Object> result = new HashMap<>();

        AppUserDto currentUser = appUserService.getCurrentUserInfo();
        result.put("currentUser", currentUser);

        Map<String, Boolean> rolePermissions = new HashMap<>();
        rolePermissions.put("isSystemAdmin", AuthorizationUtils.isSystemAdmin());
        rolePermissions.put("isDatabaseAdmin", AuthorizationUtils.isAdmin());
        rolePermissions.put("isDatabaseViewer", AuthorizationUtils.isViewer());
        rolePermissions.put("hasUserManagementAccess", AuthorizationUtils.hasUserManagementAccess());
        result.put("rolePermissions", rolePermissions);

        Map<String, Boolean> dbPermissions = new HashMap<>();
        dbPermissions.put("hasDbAccess", AuthorizationUtils.hasDbAccess());
        dbPermissions.put("hasDbReadAccess", AuthorizationUtils.hasDbReadAccess());
        dbPermissions.put("hasDbWriteAccess", AuthorizationUtils.hasDbWriteAccess());
        result.put("databasePermissions", dbPermissions);

        // Granular permissions (if schema/table specified)
        if (schemaName != null) {
            Map<String, Boolean> granularPermissions = new HashMap<>();
            granularPermissions.put("canRead", AuthorizationUtils.hasPermission(
                    PermissionType.READ, schemaName, tableName));
            granularPermissions.put("canWrite", AuthorizationUtils.hasPermission(
                    PermissionType.WRITE, schemaName, tableName));
            granularPermissions.put("canCreate", AuthorizationUtils.hasPermission(
                    PermissionType.CREATE, schemaName, tableName));
            granularPermissions.put("canDelete", AuthorizationUtils.hasPermission(
                    PermissionType.DELETE, schemaName, tableName));

            result.put("granularPermissions", granularPermissions);
            result.put("targetSchema", schemaName);
            result.put("targetTable", tableName);
        }

        return ResponseEntity.ok(ApiResponse.<Map<String, Object>>builder()
                .message("Detailed permissions retrieved successfully")
                .success(true)
                .data(result)
                .build());
    }
}