package ma.ilias.dbmanagementbe.controller;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import lombok.Getter;
import lombok.Setter;
import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.service.TestTargetDbService;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api")
public class TestController {

    private final AuthenticationManager authenticationManager;
    private final TestTargetDbService targetDbService;
    private final AppUserRepository appUserRepository;

    public TestController(
            AuthenticationManager authenticationManager,
            TestTargetDbService targetDbService,
            AppUserRepository appUserRepository
    ) {
        this.authenticationManager = authenticationManager;
        this.targetDbService = targetDbService;
        this.appUserRepository = appUserRepository;
    }

    @PostMapping("/login")
    public ResponseEntity<String> login(
            @RequestBody LoginRequest loginRequest,
            HttpServletRequest request
    ) {
        Authentication authentication = authenticationManager.authenticate(
                new UsernamePasswordAuthenticationToken(
                        loginRequest.getUsername(),
                        loginRequest.getPassword()
                )
        );

        SecurityContextHolder.getContext().setAuthentication(authentication);

        HttpSession session = request.getSession(false);
        if (session != null) {
            session.invalidate();
        }
        HttpSession newSession = request.getSession(true);
        newSession.setAttribute("SPRING_SECURITY_CONTEXT", SecurityContextHolder.getContext());

        return ResponseEntity.ok("Logged in as: " + authentication.getName());
    }

    @GetMapping("/admin/dashboard")
    public ResponseEntity<String> adminDashboard() {
        return ResponseEntity.ok("Welcome to Admin Dashboard");
    }

    @GetMapping("/viewer/dashboard")
    public ResponseEntity<String> viewerDashboard() {
        return ResponseEntity.ok("Welcome to Viewer Dashboard");
    }

    @GetMapping("/public")
    public ResponseEntity<String> publicEndpoint() {
        return ResponseEntity.ok("Public API Endpoint");
    }

    @GetMapping("/databases")
    public List<String> listTables() {
        return targetDbService.getDatabases();
    }

    @GetMapping("/users")
    public List<AppUser> listUsers() {
        return appUserRepository.findAll();
    }
}

@Getter
@Setter
class LoginRequest {
    private String username;
    private String password;
}