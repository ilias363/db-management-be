package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import jakarta.validation.groups.Default;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.NewPrimaryKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.NewStandardColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.*;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.groups.StandaloneColumnCreation;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

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
        BaseColumnMetadataDto column = columnService.getColumn(schemaName, tableName, columnName, true, true);
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

    @PatchMapping("/data-type")
    public ResponseEntity<ApiResponse<BaseColumnMetadataDto>> updateColumnDataType(
            @Valid @RequestBody UpdateColumnDataTypeDto updateColumnDataTypeDto
    ) {
        BaseColumnMetadataDto updatedColumn = columnService.updateColumnDataType(updateColumnDataTypeDto);
        return ResponseEntity.ok(ApiResponse.<BaseColumnMetadataDto>builder()
                .message("Column data type updated successfully")
                .success(true)
                .data(updatedColumn)
                .build());
    }

    @PatchMapping("/auto-increment")
    public ResponseEntity<ApiResponse<BaseColumnMetadataDto>> updateColumnAutoIncrement(
            @Valid @RequestBody UpdateColumnAutoIncrementDto updateColumnAutoIncrementDto
    ) {
        BaseColumnMetadataDto updatedColumn = columnService.updateColumnAutoIncrement(updateColumnAutoIncrementDto);
        return ResponseEntity.ok(ApiResponse.<BaseColumnMetadataDto>builder()
                .message("Column auto increment updated successfully")
                .success(true)
                .data(updatedColumn)
                .build());
    }

    @PatchMapping("/nullable")
    public ResponseEntity<ApiResponse<BaseColumnMetadataDto>> updateColumnNullable(
            @Valid @RequestBody UpdateColumnNullableDto updateColumnNullableDto,
            @RequestParam(defaultValue = "false") boolean populate
    ) {
        BaseColumnMetadataDto updatedColumn = columnService.updateColumnNullable(updateColumnNullableDto, populate);
        return ResponseEntity.ok(ApiResponse.<BaseColumnMetadataDto>builder()
                .message("Column nullable constraint updated successfully")
                .success(true)
                .data(updatedColumn)
                .build());
    }

    @PatchMapping("/unique")
    public ResponseEntity<ApiResponse<BaseColumnMetadataDto>> updateColumnUnique(
            @Valid @RequestBody UpdateColumnUniqueDto updateColumnUniqueDto
    ) {
        BaseColumnMetadataDto updatedColumn = columnService.updateColumnUnique(updateColumnUniqueDto);
        return ResponseEntity.ok(ApiResponse.<BaseColumnMetadataDto>builder()
                .message("Column unique constraint updated successfully")
                .success(true)
                .data(updatedColumn)
                .build());
    }

    @PatchMapping("/default")
    public ResponseEntity<ApiResponse<BaseColumnMetadataDto>> updateColumnDefault(
            @Valid @RequestBody UpdateColumnDefaultDto updateColumnDefaultDto
    ) {
        BaseColumnMetadataDto updatedColumn = columnService.updateColumnDefault(updateColumnDefaultDto);
        return ResponseEntity.ok(ApiResponse.<BaseColumnMetadataDto>builder()
                .message("Column default value updated successfully")
                .success(true)
                .data(updatedColumn)
                .build());
    }

    @PatchMapping("/primary-key")
    public ResponseEntity<ApiResponse<List<BaseColumnMetadataDto>>> updateColumnPrimaryKey(
            @Valid @RequestBody UpdateColumnPrimaryKeyDto updateColumnPrimaryKeyDto,
            @RequestParam(defaultValue = "false") boolean force
    ) {
        List<BaseColumnMetadataDto> updatedColumns = columnService.updateColumnPrimaryKey(updateColumnPrimaryKeyDto, force);
        return ResponseEntity.ok(ApiResponse.<List<BaseColumnMetadataDto>>builder()
                .message("Column primary key constraint updated successfully")
                .success(true)
                .data(updatedColumns)
                .build());
    }

    @PatchMapping("/foreign-key")
    public ResponseEntity<ApiResponse<BaseColumnMetadataDto>> updateColumnForeignKey(
            @Valid @RequestBody UpdateColumnForeignKeyDto updateColumnForeignKeyDto
    ) {
        BaseColumnMetadataDto updatedColumn = columnService.updateColumnForeignKey(updateColumnForeignKeyDto);
        return ResponseEntity.ok(ApiResponse.<BaseColumnMetadataDto>builder()
                .message("Column foreign key constraint updated successfully")
                .success(true)
                .data(updatedColumn)
                .build());
    }
}