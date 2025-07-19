package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.metadata.dto.table.NewTableDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.UpdateTableDto;
import ma.ilias.dbmanagementbe.metadata.service.table.TableService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/tables")
@AllArgsConstructor
public class TableController {

    private final TableService tableService;

    @GetMapping("/{schemaName}/{tableName}")
    public ResponseEntity<ApiResponse<TableMetadataDto>> getTable(
            @PathVariable String schemaName,
            @PathVariable String tableName
    ) {
        TableMetadataDto table = tableService.getTable(schemaName, tableName, true, true);
        return ResponseEntity.ok(ApiResponse.<TableMetadataDto>builder()
                .message("Table fetched successfully")
                .success(true)
                .data(table)
                .build());
    }

    @GetMapping("/{schemaName}")
    public ResponseEntity<ApiResponse<List<TableMetadataDto>>> getAllTablesInSchema(@PathVariable String schemaName) {
        List<TableMetadataDto> tables = tableService.getTablesBySchema(schemaName);
        return ResponseEntity.ok(ApiResponse.<List<TableMetadataDto>>builder()
                .message("Tables fetched successfully")
                .success(true)
                .data(tables)
                .build());
    }

    @PostMapping
    public ResponseEntity<ApiResponse<TableMetadataDto>> createTable(@Valid @RequestBody NewTableDto newTableDto) {
        TableMetadataDto createdTable = tableService.createTable(newTableDto);
        return new ResponseEntity<>(ApiResponse.<TableMetadataDto>builder()
                .message("Table created successfully")
                .success(true)
                .data(createdTable)
                .build(), HttpStatus.CREATED);
    }


    @PutMapping
    public ResponseEntity<ApiResponse<TableMetadataDto>> renameTable(
            @Valid @RequestBody UpdateTableDto updateTableDto
    ) {
        TableMetadataDto updatedTable = tableService.renameTable(updateTableDto);
        return ResponseEntity.ok(ApiResponse.<TableMetadataDto>builder()
                .message("Table updated successfully")
                .success(true)
                .data(updatedTable)
                .build());
    }

    @DeleteMapping("/{schemaName}/{tableName}")
    public ResponseEntity<ApiResponse<Void>> deleteTable(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @RequestParam(defaultValue = "false") boolean force
    ) {
        return tableService.deleteTable(schemaName, tableName, force) ?
                ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Table deleted successfully")
                        .success(true)
                        .build())
                :
                ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Table has not been deleted")
                        .success(false)
                        .build());
    }
}
