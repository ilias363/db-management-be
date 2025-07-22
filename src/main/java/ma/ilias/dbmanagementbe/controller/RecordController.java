package ma.ilias.dbmanagementbe.controller;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.record.dto.RecordPageDto;
import ma.ilias.dbmanagementbe.record.service.RecordService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/records")
@AllArgsConstructor
public class RecordController {

    private final RecordService recordService;

    @GetMapping("/{schemaName}/{tableName}")
    public ResponseEntity<ApiResponse<RecordPageDto>> getRecords(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(required = false) String sortBy,
            @RequestParam(defaultValue = "ASC") String sortDirection
    ) {
        RecordPageDto records = recordService.getRecords(schemaName, tableName, page, size, sortBy, sortDirection);
        return ResponseEntity.ok(ApiResponse.<RecordPageDto>builder()
                .message("Records fetched successfully")
                .success(true)
                .data(records)
                .build());
    }

    @GetMapping("/{schemaName}/{tableName}/count")
    public ResponseEntity<ApiResponse<Long>> getRecordCount(
            @PathVariable String schemaName,
            @PathVariable String tableName
    ) {
        long count = recordService.getRecordCount(schemaName, tableName, true);
        return ResponseEntity.ok(ApiResponse.<Long>builder()
                .message("Record count fetched successfully")
                .success(true)
                .data(count)
                .build());
    }
}
