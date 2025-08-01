package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.RefreshToken;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.dto.appuser.AppUserDto;
import ma.ilias.dbmanagementbe.dto.auth.LoginRequestDto;
import ma.ilias.dbmanagementbe.dto.auth.LoginResponseDto;
import ma.ilias.dbmanagementbe.dto.auth.TokenRefreshRequestDto;
import ma.ilias.dbmanagementbe.dto.auth.TokenRefreshResponseDto;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.enums.PermissionType;
import ma.ilias.dbmanagementbe.exception.TokenRefreshException;
import ma.ilias.dbmanagementbe.service.AppUserService;
import ma.ilias.dbmanagementbe.service.AuditService;
import ma.ilias.dbmanagementbe.service.JwtService;
import ma.ilias.dbmanagementbe.service.RefreshTokenService;
import ma.ilias.dbmanagementbe.util.AuthorizationUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.time.ZoneId;
import java.util.HashMap;
import java.util.Map;

@RestController
@RequiredArgsConstructor
@RequestMapping("api/auth")
public class AuthController {

    private final AuthenticationManager authenticationManager;
    private final AuditService auditService;
    private final AppUserService appUserService;
    private final JwtService jwtService;
    private final RefreshTokenService refreshTokenService;

    @PostMapping("/login")
    public ResponseEntity<ApiResponse<LoginResponseDto>> login(@Valid @RequestBody LoginRequestDto loginRequestDto) {
        try {
            authenticationManager.authenticate(
                    new UsernamePasswordAuthenticationToken(
                            loginRequestDto.getUsername(),
                            loginRequestDto.getPassword()
                    )
            );

            AppUserDto user = appUserService.findByUsername(loginRequestDto.getUsername(), false);

            String accessToken = jwtService.generateToken(user.getId().toString());
            RefreshToken refreshToken = refreshTokenService.createRefreshToken(user.getId());

            auditService.auditSuccessfulAction(ActionType.LOGIN, user.getUsername() + " (ID: " + user.getId() + ")");

            LoginResponseDto jwtResponse = LoginResponseDto.builder()
                    .accessToken(accessToken)
                    .refreshToken(refreshToken.getToken())
                    .accessTokenExpiry(jwtService.extractExpiration(accessToken).getTime())
                    .refreshTokenExpiry(refreshToken
                            .getExpiryDate()
                            .atZone(ZoneId.systemDefault())
                            .toInstant()
                            .toEpochMilli())
                    .build();

            return ResponseEntity.ok(ApiResponse.<LoginResponseDto>builder()
                    .message("Login successful")
                    .success(true)
                    .data(jwtResponse)
                    .build());
        } catch (BadCredentialsException ex) {
            AppUserDto user = appUserService.findByUsername(loginRequestDto.getUsername(), false);
            auditService.auditFailedAction(ActionType.LOGIN,
                    user.getUsername() + " (ID: " + user.getId() + ")",
                    "Invalid credentials");

            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(
                    ApiResponse.<LoginResponseDto>builder()
                            .message("Invalid credentials")
                            .success(false)
                            .build()
            );
        } catch (Exception ex) {
            AppUserDto user = appUserService.findByUsername(loginRequestDto.getUsername(), false);
            auditService.auditFailedAction(ActionType.LOGIN,
                    user.getUsername() + " (ID: " + user.getId() + ")",
                    "Authentication failed: " + ex.getMessage());

            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(
                    ApiResponse.<LoginResponseDto>builder()
                            .message("Authentication failed : " + ex.getMessage())
                            .success(false)
                            .build()
            );
        }
    }

    @PostMapping("/logout")
    public ResponseEntity<ApiResponse<Void>> logout() {
        // Get current user before clearing context for audit
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        String username = (auth != null && auth.isAuthenticated()) ? auth.getName() : "unknown";

        if (!username.equals("unknown")) {
            AppUserDto user = appUserService.findByUsername(username, false);
            // Delete refresh tokens for this user
            refreshTokenService.deleteByUserId(user.getId());
            auditService.auditSuccessfulAction(ActionType.LOGOUT, username + " (ID: " + user.getId() + ")");
        } else {
            auditService.auditSuccessfulAction(ActionType.LOGOUT, username);
        }

        SecurityContextHolder.clearContext();

        return ResponseEntity.ok(ApiResponse.<Void>builder()
                .message("Logout successful")
                .success(true)
                .build());
    }

    @PostMapping("/refresh")
    public ResponseEntity<ApiResponse<TokenRefreshResponseDto>> refreshToken(
            @Valid @RequestBody TokenRefreshRequestDto request) {
        try {
            String requestRefreshToken = request.getRefreshToken();

            return refreshTokenService.findByToken(requestRefreshToken)
                    .map(refreshTokenService::verifyExpiration)
                    .map(RefreshToken::getUser)
                    .map(user -> {
                        String newAccessToken = jwtService.generateToken(user.getId().toString());
                        RefreshToken newRefreshToken = refreshTokenService.createRefreshToken(user.getId());

                        TokenRefreshResponseDto response = new TokenRefreshResponseDto(
                                newAccessToken,
                                newRefreshToken.getToken()
                        );

                        auditService.auditSuccessfulAction(ActionType.LOGIN,
                                user.getUsername() + " (ID: " + user.getId() + ") - Token refresh");

                        return ResponseEntity.ok(ApiResponse.<TokenRefreshResponseDto>builder()
                                .message("Token refreshed successfully")
                                .success(true)
                                .data(response)
                                .build());
                    })
                    .orElseThrow(() -> new TokenRefreshException(requestRefreshToken, "Refresh token does not exist in database!"));

        } catch (TokenRefreshException ex) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).body(
                    ApiResponse.<TokenRefreshResponseDto>builder()
                            .message(ex.getMessage())
                            .success(false)
                            .build()
            );
        } catch (Exception ex) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(
                    ApiResponse.<TokenRefreshResponseDto>builder()
                            .message("Token refresh failed: " + ex.getMessage())
                            .success(false)
                            .build()
            );
        }
    }

    @GetMapping("/validate")
    public ResponseEntity<ApiResponse<Map<String, Boolean>>> validateToken() {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        boolean isValid = auth != null && auth.isAuthenticated() && !"anonymousUser".equals(auth.getName());

        return ResponseEntity.ok(
                ApiResponse.<Map<String, Boolean>>builder()
                        .message("Token validation successful")
                        .success(true)
                        .data(Map.of("isValid", isValid))
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
