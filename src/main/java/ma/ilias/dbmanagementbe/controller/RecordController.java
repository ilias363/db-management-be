package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.record.dto.*;
import ma.ilias.dbmanagementbe.record.service.RecordService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

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

    @GetMapping("/{schemaName}/{tableName}/record")
    public ResponseEntity<ApiResponse<RecordDto>> getRecord(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @RequestParam Map<String, Object> primaryKeyValues
    ) {
        RecordDto record = recordService.getRecord(schemaName, tableName, primaryKeyValues);
        return ResponseEntity.ok(ApiResponse.<RecordDto>builder()
                .message("Record fetched successfully")
                .success(true)
                .data(record)
                .build());
    }

    @PostMapping
    public ResponseEntity<ApiResponse<RecordDto>> createRecord(
            @Valid @RequestBody NewRecordDto newRecordDto
    ) {
        RecordDto createdRecord = recordService.createRecord(newRecordDto);
        return new ResponseEntity<>(ApiResponse.<RecordDto>builder()
                .message("Record created successfully")
                .success(true)
                .data(createdRecord)
                .build(), HttpStatus.CREATED);
    }

    @PutMapping
    public ResponseEntity<ApiResponse<RecordDto>> updateRecord(
            @Valid @RequestBody UpdateRecordDto updateRecordDto
    ) {
        RecordDto updatedRecord = recordService.updateRecord(updateRecordDto);
        return ResponseEntity.ok(ApiResponse.<RecordDto>builder()
                .message("Record updated successfully")
                .success(true)
                .data(updatedRecord)
                .build());
    }

    @DeleteMapping("/{schemaName}/{tableName}")
    public ResponseEntity<ApiResponse<Void>> deleteRecord(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @RequestParam Map<String, Object> primaryKeyValues
    ) {
        boolean deleted = recordService.deleteRecord(schemaName, tableName, primaryKeyValues);
        return ResponseEntity.ok(ApiResponse.<Void>builder()
                .message(deleted ? "Record deleted successfully" : "Record not found")
                .success(deleted)
                .build());
    }

    @GetMapping("/{schemaName}/{tableName}/record/by-values")
    public ResponseEntity<ApiResponse<RecordDto>> getRecordByValues(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @RequestParam Map<String, Object> identifyingValues
    ) {
        RecordDto record = recordService.getRecordByValues(schemaName, tableName, identifyingValues);
        return ResponseEntity.ok(ApiResponse.<RecordDto>builder()
                .message("Record fetched successfully using identifying values")
                .success(true)
                .data(record)
                .build());
    }

    @PutMapping("/by-values")
    public ResponseEntity<ApiResponse<RecordDto>> updateRecordByValues(
            @Valid @RequestBody UpdateRecordByValuesDto updateDto
    ) {
        RecordDto updatedRecord = recordService.updateRecordByValues(updateDto);
        return ResponseEntity.ok(ApiResponse.<RecordDto>builder()
                .message("Record updated successfully using identifying values")
                .success(true)
                .data(updatedRecord)
                .build());
    }

    @DeleteMapping("/by-values")
    public ResponseEntity<ApiResponse<Integer>> deleteRecordByValues(
            @Valid @RequestBody DeleteRecordByValuesDto deleteDto
    ) {
        int deletedCount = recordService.deleteRecordByValues(deleteDto);
        return ResponseEntity.ok(ApiResponse.<Integer>builder()
                .message(deletedCount + " record(s) deleted successfully using identifying values")
                .success(true)
                .data(deletedCount)
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
