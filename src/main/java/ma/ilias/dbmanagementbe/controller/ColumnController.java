package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import jakarta.validation.groups.Default;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.NewPrimaryKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.NewStandardColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.RenameColumnDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.groups.StandaloneColumnCreation;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/columns")
@AllArgsConstructor
public class ColumnController {

    private ColumnService columnService;

    @GetMapping("/{schemaName}/{tableName}/{columnName}")
    public ResponseEntity<ApiResponse<BaseColumnMetadataDto>> getColumn(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @PathVariable String columnName
    ) {
        BaseColumnMetadataDto column = columnService.getColumn(schemaName, tableName, columnName);
        return ResponseEntity.ok(ApiResponse.<BaseColumnMetadataDto>builder()
                .message("Column fetched successfully")
                .success(true)
                .data(column)
                .build());
    }

    @PostMapping("/standard")
    public ResponseEntity<ApiResponse<BaseColumnMetadataDto>> createStandardColumn(
            @Validated({StandaloneColumnCreation.class, Default.class}) @RequestBody NewStandardColumnDto newStandardColumnDto
    ) {
        BaseColumnMetadataDto createdColumn = columnService.createColumn(newStandardColumnDto);
        return new ResponseEntity<>(ApiResponse.<BaseColumnMetadataDto>builder()
                .message("Standard column created successfully")
                .success(true)
                .data(createdColumn)
                .build(), HttpStatus.CREATED);
    }

    @PostMapping("/foreign-key")
    public ResponseEntity<ApiResponse<BaseColumnMetadataDto>> createForeignKeyColumn(
            @Validated({StandaloneColumnCreation.class, Default.class}) @RequestBody NewForeignKeyColumnDto newForeignKeyColumnDto
    ) {
        BaseColumnMetadataDto createdColumn = columnService.createColumn(newForeignKeyColumnDto);
        return new ResponseEntity<>(ApiResponse.<BaseColumnMetadataDto>builder()
                .message("Foreign key column created successfully")
                .success(true)
                .data(createdColumn)
                .build(), HttpStatus.CREATED);
    }

    @PostMapping("/primary-key")
    public ResponseEntity<ApiResponse<BaseColumnMetadataDto>> createPrimaryKeyColumn(
            @Validated({StandaloneColumnCreation.class, Default.class}) @RequestBody NewPrimaryKeyColumnDto newPrimaryKeyColumnDto
    ) {
        BaseColumnMetadataDto createdColumn = columnService.createColumn(newPrimaryKeyColumnDto);
        return new ResponseEntity<>(ApiResponse.<BaseColumnMetadataDto>builder()
                .message("Primary key column created successfully")
                .success(true)
                .data(createdColumn)
                .build(), HttpStatus.CREATED);
    }

    @DeleteMapping("/{schemaName}/{tableName}/{columnName}")
    public ResponseEntity<ApiResponse<Void>> deleteColumn(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @PathVariable String columnName,
            @RequestParam(defaultValue = "false") boolean force
    ) {
        return columnService.deleteColumn(schemaName, tableName, columnName, force) ?
                ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Column deleted successfully")
                        .success(true)
                        .build())
                :
                ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Column has not been deleted")
                        .success(false)
                        .build());
    }

    @PatchMapping("/rename")
    public ResponseEntity<ApiResponse<BaseColumnMetadataDto>> renameColumn(
            @Valid @RequestBody RenameColumnDto renameColumnDto
    ) {
        BaseColumnMetadataDto updatedColumn = columnService.renameColumn(renameColumnDto);
        return ResponseEntity.ok(ApiResponse.<BaseColumnMetadataDto>builder()
                .message("Column renamed successfully")
                .success(true)
                .data(updatedColumn)
                .build());
    }
}