package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import jakarta.validation.groups.Default;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.metadata.dto.table.NewTableDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.UpdateTableDto;
import ma.ilias.dbmanagementbe.metadata.service.table.TableService;
import ma.ilias.dbmanagementbe.service.AuditService;
import ma.ilias.dbmanagementbe.validation.groups.NotStandaloneColumnCreation;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/tables")
@AllArgsConstructor
public class TableController {

    private final TableService tableService;
    private final AuditService auditService;

    @GetMapping("/{schemaName}/{tableName}")
    public ResponseEntity<ApiResponse<TableMetadataDto>> getTable(
            @PathVariable String schemaName,
            @PathVariable String tableName
    ) {
        TableMetadataDto table = tableService.getTable(schemaName, tableName, true,
                true, true, true, true);
        return ResponseEntity.ok(ApiResponse.<TableMetadataDto>builder()
                .message("Table fetched successfully")
                .success(true)
                .data(table)
                .build());
    }

    @GetMapping("/{schemaName}")
    public ResponseEntity<ApiResponse<List<TableMetadataDto>>> getAllTablesInSchema(@PathVariable String schemaName) {
        List<TableMetadataDto> tables = tableService.getTablesBySchema(schemaName, true, true, true, true);
        return ResponseEntity.ok(ApiResponse.<List<TableMetadataDto>>builder()
                .message("Tables fetched successfully")
                .success(true)
                .data(tables)
                .build());
    }

    @PostMapping
    public ResponseEntity<ApiResponse<TableMetadataDto>> createTable(
            @Validated({NotStandaloneColumnCreation.class, Default.class}) @RequestBody NewTableDto newTableDto) {
        try {
            TableMetadataDto createdTable = tableService.createTable(newTableDto);

            auditService.auditSuccessfulAction(ActionType.CREATE_TABLE, newTableDto.getSchemaName(),
                    newTableDto.getTableName(), null);

            return new ResponseEntity<>(ApiResponse.<TableMetadataDto>builder()
                    .message("Table created successfully")
                    .success(true)
                    .data(createdTable)
                    .build(), HttpStatus.CREATED);
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.CREATE_TABLE, newTableDto.getSchemaName(),
                    newTableDto.getTableName(), null, e.getMessage());
            throw e;
        }
    }


    @PutMapping
    public ResponseEntity<ApiResponse<TableMetadataDto>> renameTable(
            @Valid @RequestBody UpdateTableDto updateTableDto
    ) {
        try {
            TableMetadataDto updatedTable = tableService.renameTable(updateTableDto);

            auditService.auditSuccessfulAction(ActionType.UPDATE_TABLE, updateTableDto.getSchemaName(),
                    updateTableDto.getTableName(), null);

            return ResponseEntity.ok(ApiResponse.<TableMetadataDto>builder()
                    .message("Table updated successfully")
                    .success(true)
                    .data(updatedTable)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.UPDATE_TABLE, updateTableDto.getSchemaName(),
                    updateTableDto.getTableName(), null, e.getMessage());
            throw e;
        }
    }

    @DeleteMapping("/{schemaName}/{tableName}")
    public ResponseEntity<ApiResponse<Void>> deleteTable(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @RequestParam(defaultValue = "false") boolean force
    ) {
        try {
            boolean deleted = tableService.deleteTable(schemaName, tableName, force);

            if (deleted) {
                auditService.auditSuccessfulAction(ActionType.DELETE_TABLE, schemaName, tableName);

                return ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Table deleted successfully")
                        .success(true)
                        .build());
            } else {
                auditService.auditFailedAction(ActionType.DELETE_TABLE, schemaName, tableName, "Table deletion failed");

                return ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Table has not been deleted")
                        .success(false)
                        .build());
            }
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.DELETE_TABLE, schemaName, tableName, e.getMessage());
            throw e;
        }
    }
}
