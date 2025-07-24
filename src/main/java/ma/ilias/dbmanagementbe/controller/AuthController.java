package ma.ilias.dbmanagementbe.controller;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.dto.auth.LoginRequestDto;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.service.AuditService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@AllArgsConstructor
@RestController
@RequestMapping("api/auth")
public class AuthController {

    private final AuthenticationManager authenticationManager;
    private final AuditService auditService;

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

            auditService.auditSuccessfulAction(ActionType.LOGIN, loginRequestDto.getUsername());

            return ResponseEntity.ok(ApiResponse.<Void>builder()
                    .message("Login successful")
                    .success(true)
                    .build());
        } catch (BadCredentialsException ex) {
            auditService.auditFailedAction(ActionType.LOGIN, loginRequestDto.getUsername(), "Invalid credentials");

            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(
                    ApiResponse.<Void>builder()
                            .message("Invalid credentials")
                            .success(false)
                            .build()
            );
        } catch (Exception ex) {
            auditService.auditFailedAction(ActionType.LOGIN, loginRequestDto.getUsername(), "Authentication failed: " + ex.getMessage());

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

        auditService.auditSuccessfulAction(ActionType.LOGOUT, username);

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
}