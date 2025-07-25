package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Pattern;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.record.dto.*;
import ma.ilias.dbmanagementbe.record.service.RecordService;
import ma.ilias.dbmanagementbe.service.AuditService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/records")
@AllArgsConstructor
public class RecordController {

    private final RecordService recordService;
    private final AuditService auditService;

    @GetMapping("/{schemaName}/{tableName}")
    public ResponseEntity<ApiResponse<RecordPageDto>> getRecords(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) @Max(100) int size,
            @RequestParam(required = false) String sortBy,
            @RequestParam(defaultValue = "ASC") @Pattern(regexp = "^(ASC|DESC)$",
                    message = "Sort direction must be either ASC or DESC") String sortDirection
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
        try {
            RecordDto createdRecord = recordService.createRecord(newRecordDto);

            auditService.auditSuccessfulAction(
                    ActionType.CREATE_RECORD, createdRecord.getSchemaName(), createdRecord.getTableName());

            return new ResponseEntity<>(ApiResponse.<RecordDto>builder()
                    .message("Record created successfully")
                    .success(true)
                    .data(createdRecord)
                    .build(), HttpStatus.CREATED);
        } catch (Exception e) {
            auditService.auditFailedAction(
                    ActionType.CREATE_RECORD, newRecordDto.getSchemaName(),
                    newRecordDto.getTableName(), e.getMessage());
            throw e;
        }
    }

    @PutMapping
    public ResponseEntity<ApiResponse<RecordDto>> updateRecord(
            @Valid @RequestBody UpdateRecordDto updateRecordDto
    ) {
        try {
            RecordDto updatedRecord = recordService.updateRecord(updateRecordDto);

            auditService.auditSuccessfulAction(
                    ActionType.UPDATE_RECORD, updatedRecord.getSchemaName(), updatedRecord.getTableName());

            return ResponseEntity.ok(ApiResponse.<RecordDto>builder()
                    .message("Record updated successfully")
                    .success(true)
                    .data(updatedRecord)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(
                    ActionType.UPDATE_RECORD, updateRecordDto.getSchemaName(),
                    updateRecordDto.getTableName(), e.getMessage());
            throw e;
        }
    }

    @DeleteMapping("/{schemaName}/{tableName}")
    public ResponseEntity<ApiResponse<Void>> deleteRecord(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @RequestParam Map<String, Object> primaryKeyValues
    ) {
        try {
            boolean deleted = recordService.deleteRecord(schemaName, tableName, primaryKeyValues);

            if (deleted) {
                auditService.auditSuccessfulAction(ActionType.DELETE_RECORD, schemaName, tableName);
            } else {
                auditService.auditFailedAction(ActionType.DELETE_RECORD, schemaName, tableName, "Record not found");
            }

            return ResponseEntity.ok(ApiResponse.<Void>builder()
                    .message(deleted ? "Record deleted successfully" : "Record not found")
                    .success(deleted)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.DELETE_RECORD, schemaName, tableName, e.getMessage());
            throw e;
        }
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

    @GetMapping("/{schemaName}/{tableName}/records/by-values")
    public ResponseEntity<ApiResponse<List<RecordDto>>> getRecordsByValues(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @RequestParam Map<String, Object> identifyingValues,
            @RequestParam(defaultValue = "false") boolean limitOne
    ) {
        List<RecordDto> records = recordService.getRecordsByValues(schemaName, tableName, identifyingValues, limitOne);
        return ResponseEntity.ok(ApiResponse.<List<RecordDto>>builder()
                .message(records.size() + " record(s) fetched successfully using identifying values")
                .success(true)
                .data(records)
                .build());
    }

    @PutMapping("/by-values")
    public ResponseEntity<ApiResponse<List<RecordDto>>> updateRecordByValues(
            @Valid @RequestBody UpdateRecordByValuesDto updateDto
    ) {
        try {
            List<RecordDto> updatedRecords = recordService.updateRecordByValues(updateDto);

            if (updatedRecords.size() == 1) {
                auditService.auditSuccessfulAction(ActionType.UPDATE_RECORD,
                        updateDto.getSchemaName(), updateDto.getTableName());
            } else if (!updatedRecords.isEmpty()) {
                auditService.auditSuccessfulAction(ActionType.UPDATE_MULTIPLE_RECORDS,
                        updateDto.getSchemaName(), updateDto.getTableName());
            }

            return ResponseEntity.ok(ApiResponse.<List<RecordDto>>builder()
                    .message(updatedRecords.size() + " record(s) updated successfully using identifying values")
                    .success(true)
                    .data(updatedRecords)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(
                    ActionType.UPDATE_RECORD, updateDto.getSchemaName(),
                    updateDto.getTableName(), e.getMessage());
            throw e;
        }
    }

    @DeleteMapping("/by-values")
    public ResponseEntity<ApiResponse<Integer>> deleteRecordByValues(
            @Valid @RequestBody DeleteRecordByValuesDto deleteDto
    ) {
        try {
            int deletedCount = recordService.deleteRecordByValues(deleteDto);

            if (deletedCount == 1) {
                auditService.auditSuccessfulAction(ActionType.DELETE_RECORD,
                        deleteDto.getSchemaName(), deleteDto.getTableName());
            } else if (deletedCount > 1) {
                auditService.auditSuccessfulAction(ActionType.DELETE_MULTIPLE_RECORDS,
                        deleteDto.getSchemaName(), deleteDto.getTableName());
            }

            return ResponseEntity.ok(ApiResponse.<Integer>builder()
                    .message(deletedCount + " record(s) deleted successfully using identifying values")
                    .success(true)
                    .data(deletedCount)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.DELETE_RECORD, deleteDto.getSchemaName(), deleteDto.getTableName());
            throw e;
        }
    }

    @PostMapping("/batch")
    public ResponseEntity<ApiResponse<List<RecordDto>>> createRecords(
            @Valid @RequestBody BatchNewRecordsDto batchNewRecords
    ) {
        try {
            List<RecordDto> createdRecords = recordService.createRecords(batchNewRecords);

            if (createdRecords.size() == 1) {
                auditService.auditSuccessfulAction(ActionType.UPDATE_RECORD,
                        batchNewRecords.getSchemaName(), batchNewRecords.getTableName());
            } else if (!createdRecords.isEmpty()) {
                auditService.auditSuccessfulAction(ActionType.UPDATE_MULTIPLE_RECORDS,
                        batchNewRecords.getSchemaName(), batchNewRecords.getTableName());
            }

            return new ResponseEntity<>(ApiResponse.<List<RecordDto>>builder()
                    .message(createdRecords.size() + " records created successfully")
                    .success(true)
                    .data(createdRecords)
                    .build(), HttpStatus.CREATED);
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.CREATE_MULTIPLE_RECORDS, batchNewRecords.getSchemaName(),
                    batchNewRecords.getTableName(), e.getMessage());
            throw e;
        }
    }

    @PutMapping("/batch")
    public ResponseEntity<ApiResponse<List<RecordDto>>> updateRecords(
            @Valid @RequestBody BatchUpdateRecordsDto batchUpdateRecords
    ) {
        try {
            List<RecordDto> updatedRecords = recordService.updateRecords(batchUpdateRecords);

            if (updatedRecords.size() == 1) {
                auditService.auditSuccessfulAction(ActionType.UPDATE_RECORD,
                        batchUpdateRecords.getSchemaName(), batchUpdateRecords.getTableName());
            } else if (!updatedRecords.isEmpty()) {
                auditService.auditSuccessfulAction(ActionType.UPDATE_MULTIPLE_RECORDS,
                        batchUpdateRecords.getSchemaName(), batchUpdateRecords.getTableName());
            }

            return ResponseEntity.ok(ApiResponse.<List<RecordDto>>builder()
                    .message(updatedRecords.size() + " records updated successfully")
                    .success(true)
                    .data(updatedRecords)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.UPDATE_MULTIPLE_RECORDS, batchUpdateRecords.getSchemaName(),
                    batchUpdateRecords.getTableName(), e.getMessage());
            throw e;
        }
    }

    @DeleteMapping("/batch")
    public ResponseEntity<ApiResponse<Integer>> deleteRecords(
            @Valid @RequestBody BatchDeleteRecordsDto batchDeleteRecords
    ) {
        try {
            int deletedCount = recordService.deleteRecords(batchDeleteRecords);

            if (deletedCount == 1) {
                auditService.auditSuccessfulAction(ActionType.DELETE_RECORD,
                        batchDeleteRecords.getSchemaName(), batchDeleteRecords.getTableName());
            } else if (deletedCount > 1) {
                auditService.auditSuccessfulAction(ActionType.DELETE_MULTIPLE_RECORDS,
                        batchDeleteRecords.getSchemaName(), batchDeleteRecords.getTableName());
            }

            return ResponseEntity.ok(ApiResponse.<Integer>builder()
                    .message(deletedCount + " records deleted successfully")
                    .success(true)
                    .data(deletedCount)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.DELETE_MULTIPLE_RECORDS, batchDeleteRecords.getSchemaName(),
                    batchDeleteRecords.getTableName(), e.getMessage());
            throw e;
        }
    }

    @PutMapping("/batch/by-values")
    public ResponseEntity<ApiResponse<List<RecordDto>>> updateRecordsByValues(
            @Valid @RequestBody BatchUpdateRecordsByValuesDto batchUpdateByValues
    ) {
        try {
            List<RecordDto> updatedRecords = recordService.updateRecordsByValues(batchUpdateByValues);

            if (updatedRecords.size() == 1) {
                auditService.auditSuccessfulAction(ActionType.UPDATE_RECORD,
                        batchUpdateByValues.getSchemaName(), batchUpdateByValues.getTableName());
            } else if (!updatedRecords.isEmpty()) {
                auditService.auditSuccessfulAction(ActionType.UPDATE_MULTIPLE_RECORDS,
                        batchUpdateByValues.getSchemaName(), batchUpdateByValues.getTableName());
            }

            return ResponseEntity.ok(ApiResponse.<List<RecordDto>>builder()
                    .message(updatedRecords.size() + " records updated successfully using identifying values")
                    .success(true)
                    .data(updatedRecords)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.UPDATE_MULTIPLE_RECORDS, batchUpdateByValues.getSchemaName(),
                    batchUpdateByValues.getTableName(), e.getMessage());
            throw e;
        }
    }

    @DeleteMapping("/batch/by-values")
    public ResponseEntity<ApiResponse<Integer>> deleteRecordsByValues(
            @Valid @RequestBody BatchDeleteRecordsByValuesDto batchDeleteByValues
    ) {
        try {
            int deletedCount = recordService.deleteRecordsByValues(batchDeleteByValues);

            if (deletedCount == 1) {
                auditService.auditSuccessfulAction(ActionType.DELETE_RECORD,
                        batchDeleteByValues.getSchemaName(), batchDeleteByValues.getTableName());
            } else if (deletedCount > 1) {
                auditService.auditSuccessfulAction(ActionType.DELETE_MULTIPLE_RECORDS,
                        batchDeleteByValues.getSchemaName(), batchDeleteByValues.getTableName());
            }

            return ResponseEntity.ok(ApiResponse.<Integer>builder()
                    .message(deletedCount + " records deleted successfully using identifying values")
                    .success(true)
                    .data(deletedCount)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.DELETE_MULTIPLE_RECORDS, batchDeleteByValues.getSchemaName(),
                    batchDeleteByValues.getTableName(), e.getMessage());
            throw e;
        }
    }

    @PostMapping("/advanced-search")
    public ResponseEntity<ApiResponse<AdvancedSearchResponseDto>> advancedSearch(
            @Valid @RequestBody AdvancedSearchRequestDto searchRequest
    ) {
        AdvancedSearchResponseDto searchResponse = recordService.advancedSearch(searchRequest);
        return ResponseEntity.ok(ApiResponse.<AdvancedSearchResponseDto>builder()
                .message("Advanced search completed successfully")
                .success(true)
                .data(searchResponse)
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
