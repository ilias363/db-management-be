package ma.ilias.dbmanagementbe.controller;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.database.dto.DatabaseStatsDto;
import ma.ilias.dbmanagementbe.database.dto.DatabaseTypeDto;
import ma.ilias.dbmanagementbe.database.service.DatabaseInfoService;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.enums.DatabaseType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/database")
@AllArgsConstructor
public class DatabaseInfoController {

    private final DatabaseInfoService databaseInfoService;

    @GetMapping("/type")
    public ResponseEntity<ApiResponse<DatabaseTypeDto>> getDatabaseType() {
        DatabaseType type = databaseInfoService.getDatabaseType();
        DatabaseTypeDto dto = new DatabaseTypeDto(type.name(), type.getDisplayName());
        return ResponseEntity.ok(ApiResponse.<DatabaseTypeDto>builder()
                .message("Database type fetched successfully")
                .success(true)
                .data(dto)
                .build());
    }

    @GetMapping("/stats")
    public ResponseEntity<ApiResponse<DatabaseStatsDto>> getDatabaseStats(
            @RequestParam(defaultValue = "false") boolean includeSystem
    ) {
        DatabaseStatsDto stats = databaseInfoService.getStats(includeSystem);
        return ResponseEntity.ok(ApiResponse.<DatabaseStatsDto>builder()
                .message("Database stats fetched successfully")
                .success(true)
                .data(stats)
                .build());
    }
}
