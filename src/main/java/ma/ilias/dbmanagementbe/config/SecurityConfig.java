package ma.ilias.dbmanagementbe.config;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.util.AuthorizationUtils;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfigurationSource;

@Configuration
@EnableWebSecurity
@AllArgsConstructor
public class SecurityConfig {

    private final CustomAuthenticationEntryPoint customAuthenticationEntryPoint;
    private final CorsConfigurationSource corsConfigurationSource;
    private final JwtAuthenticationFilter jwtAuthenticationFilter;

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        http
                .cors(cors -> cors.configurationSource(corsConfigurationSource))
                .csrf(AbstractHttpConfigurer::disable)
                .authorizeHttpRequests((requests) -> requests
                        // For preflight requests
                        .requestMatchers(HttpMethod.OPTIONS, "/**").permitAll()

                        // Public endpoints
                        .requestMatchers("/api/auth/login", "/api/auth/refresh").permitAll()

                        // System admin only endpoints - User/Role/Permission/Audit-logs management
                        .requestMatchers("/api/users/**").access((authentication, context) ->
                                AuthorizationUtils.createUserManagementDecision())
                        .requestMatchers("/api/roles/**").access((authentication, context) ->
                                AuthorizationUtils.createUserManagementDecision())
                        .requestMatchers("/api/permissions/**").access((authentication, context) ->
                                AuthorizationUtils.createUserManagementDecision())
                        .requestMatchers("/api/audit-logs/**").access((authentication, context) ->
                                AuthorizationUtils.createUserManagementDecision())

                        // Database read operations - ADMIN, VIEWER, or any custom role with DB read access
                        .requestMatchers(HttpMethod.GET, "/api/schemas/**").access((authentication, context) ->
                                AuthorizationUtils.createDbReadDecision())
                        .requestMatchers(HttpMethod.GET, "/api/tables/**").access((authentication, context) ->
                                AuthorizationUtils.createDbReadDecision())
                        .requestMatchers(HttpMethod.GET, "/api/views/**").access((authentication, context) ->
                                AuthorizationUtils.createDbReadDecision())
                        .requestMatchers(HttpMethod.GET, "/api/columns/**").access((authentication, context) ->
                                AuthorizationUtils.createDbReadDecision())
                        .requestMatchers(HttpMethod.GET, "/api/indexes/**").access((authentication, context) ->
                                AuthorizationUtils.createDbReadDecision())
                        .requestMatchers(HttpMethod.GET, "/api/records/**").access((authentication, context) ->
                                AuthorizationUtils.createDbReadDecision())
                        .requestMatchers(HttpMethod.GET, "/api/database/**").access((authentication, context) ->
                                AuthorizationUtils.createDbReadDecision())

                        // Database write operations - ADMIN or any custom role with DB write access
                        .requestMatchers("/api/schemas/**").access((authentication, context) ->
                                AuthorizationUtils.createDbWriteDecision())
                        .requestMatchers("/api/tables/**").access((authentication, context) ->
                                AuthorizationUtils.createDbWriteDecision())
                        .requestMatchers("/api/views/**").access((authentication, context) ->
                                AuthorizationUtils.createDbWriteDecision())
                        .requestMatchers("/api/columns/**").access((authentication, context) ->
                                AuthorizationUtils.createDbWriteDecision())
                        .requestMatchers("/api/indexes/**").access((authentication, context) ->
                                AuthorizationUtils.createDbWriteDecision())
                        .requestMatchers("/api/records/**").access((authentication, context) ->
                                AuthorizationUtils.createDbWriteDecision())

                        // All other requests require authentication
                        .anyRequest().authenticated()
                )
                .sessionManagement(session -> session
                        .sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                )
                .addFilterBefore(jwtAuthenticationFilter, UsernamePasswordAuthenticationFilter.class)
                .exceptionHandling(exception -> exception
                        .authenticationEntryPoint(customAuthenticationEntryPoint));

        return http.build();
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration authenticationConfiguration) throws Exception {
        return authenticationConfiguration.getAuthenticationManager();
    }
}
