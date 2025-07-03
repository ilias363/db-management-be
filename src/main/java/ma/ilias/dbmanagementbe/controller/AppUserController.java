package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.dto.AppUserDto;
import ma.ilias.dbmanagementbe.dto.NewAppUserDto;
import ma.ilias.dbmanagementbe.dto.UpdateAppUserDto;
import ma.ilias.dbmanagementbe.service.AppUserService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/users")
@AllArgsConstructor
public class AppUserController {

    private final AppUserService appUserService;

    @PostMapping
    public ResponseEntity<ApiResponse<AppUserDto>> createUser(@Valid @RequestBody NewAppUserDto newAppUserDto) {
        AppUserDto createdUser = appUserService.save(newAppUserDto);
        return new ResponseEntity<>(ApiResponse.<AppUserDto>builder()
                .message("User created successfully")
                .success(true)
                .data(createdUser)
                .build(), HttpStatus.CREATED);
    }

    @GetMapping("/{id}")
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
        AppUserDto appUserDto = appUserService.findByUsername(username);
        return ResponseEntity.ok(ApiResponse.<AppUserDto>builder()
                .message("User fetched successfully")
                .success(true)
                .data(appUserDto)
                .build());
    }

    @GetMapping
    public ResponseEntity<ApiResponse<List<AppUserDto>>> getAllUsers() {
        List<AppUserDto> appUsers = appUserService.findAll();
        return ResponseEntity.ok(ApiResponse.<List<AppUserDto>>builder()
                .message("Users fetched successfully")
                .success(true)
                .data(appUsers)
                .build());
    }

    @PutMapping("/{id}")
    public ResponseEntity<ApiResponse<AppUserDto>> updateUser(
            @PathVariable Long id,
            @Valid @RequestBody UpdateAppUserDto updateAppUserDto
    ) {
        AppUserDto updatedUser = appUserService.update(id, updateAppUserDto);
        return ResponseEntity.ok(ApiResponse.<AppUserDto>builder()
                .message("User updated successfully")
                .success(true)
                .data(updatedUser)
                .build());
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<ApiResponse<Void>> deleteUser(@PathVariable Long id) {
        return appUserService.deleteById(id) ?
                ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("User deleted successfully")
                        .success(true)
                        .build())
                :
                ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("User has not been deleted")
                        .success(false)
                        .build());
    }
}