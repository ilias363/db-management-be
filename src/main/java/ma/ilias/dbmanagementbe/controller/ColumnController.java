package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import jakarta.validation.groups.Default;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseTableColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.NewPrimaryKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.NewStandardColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.*;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.service.AuditService;
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
    private AuditService auditService;

    @GetMapping("/{schemaName}/{tableName}/{columnName}")
    public ResponseEntity<ApiResponse<BaseTableColumnMetadataDto>> getColumn(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @PathVariable String columnName
    ) {
        BaseTableColumnMetadataDto column = columnService.getColumn(
                schemaName, tableName, columnName,
                true, true, true);
        return ResponseEntity.ok(ApiResponse.<BaseTableColumnMetadataDto>builder()
                .message("Column fetched successfully")
                .success(true)
                .data(column)
                .build());
    }

    @GetMapping("/{schemaName}/{tableName}")
    public ResponseEntity<ApiResponse<List<BaseTableColumnMetadataDto>>> getColumnsByTable(
            @PathVariable String schemaName,
            @PathVariable String tableName
    ) {
        List<BaseTableColumnMetadataDto> columns = columnService.getColumnsByTable(schemaName, tableName, true, true);
        return ResponseEntity.ok(ApiResponse.<List<BaseTableColumnMetadataDto>>builder()
                .message("Column fetched successfully")
                .success(true)
                .data(columns)
                .build());
    }

    @PostMapping("/standard")
    public ResponseEntity<ApiResponse<BaseTableColumnMetadataDto>> createStandardColumn(
            @Validated({StandaloneColumnCreation.class, Default.class}) @RequestBody NewStandardColumnDto newStandardColumnDto
    ) {
        try {
            BaseTableColumnMetadataDto createdColumn = columnService.createColumn(newStandardColumnDto);

            auditService.auditSuccessfulAction(ActionType.CREATE_COLUMN, createdColumn.getTable().getSchema().getSchemaName(),
                    createdColumn.getTable().getTableName(), createdColumn.getColumnName());

            return new ResponseEntity<>(ApiResponse.<BaseTableColumnMetadataDto>builder()
                    .message("Standard column created successfully")
                    .success(true)
                    .data(createdColumn)
                    .build(), HttpStatus.CREATED);
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.CREATE_COLUMN, newStandardColumnDto.getSchemaName(),
                    newStandardColumnDto.getTableName(), newStandardColumnDto.getColumnName(), e.getMessage());
            throw e;
        }
    }

    @PostMapping("/foreign-key")
    public ResponseEntity<ApiResponse<BaseTableColumnMetadataDto>> createForeignKeyColumn(
            @Validated({StandaloneColumnCreation.class, Default.class}) @RequestBody NewForeignKeyColumnDto newForeignKeyColumnDto
    ) {
        try {
            BaseTableColumnMetadataDto createdColumn = columnService.createColumn(newForeignKeyColumnDto);

            auditService.auditSuccessfulAction(ActionType.CREATE_COLUMN, createdColumn.getTable().getSchema().getSchemaName(),
                    createdColumn.getTable().getTableName(), createdColumn.getColumnName());

            return new ResponseEntity<>(ApiResponse.<BaseTableColumnMetadataDto>builder()
                    .message("Foreign key column created successfully")
                    .success(true)
                    .data(createdColumn)
                    .build(), HttpStatus.CREATED);
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.CREATE_COLUMN, newForeignKeyColumnDto.getSchemaName(),
                    newForeignKeyColumnDto.getTableName(), newForeignKeyColumnDto.getColumnName(), e.getMessage());
            throw e;
        }
    }

    @PostMapping("/primary-key")
    public ResponseEntity<ApiResponse<BaseTableColumnMetadataDto>> createPrimaryKeyColumn(
            @Validated({StandaloneColumnCreation.class, Default.class}) @RequestBody NewPrimaryKeyColumnDto newPrimaryKeyColumnDto
    ) {
        try {
            BaseTableColumnMetadataDto createdColumn = columnService.createColumn(newPrimaryKeyColumnDto);

            auditService.auditSuccessfulAction(ActionType.CREATE_COLUMN, createdColumn.getTable().getSchema().getSchemaName(),
                    createdColumn.getTable().getTableName(), createdColumn.getColumnName());

            return new ResponseEntity<>(ApiResponse.<BaseTableColumnMetadataDto>builder()
                    .message("Primary key column created successfully")
                    .success(true)
                    .data(createdColumn)
                    .build(), HttpStatus.CREATED);
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.CREATE_COLUMN, newPrimaryKeyColumnDto.getSchemaName(),
                    newPrimaryKeyColumnDto.getTableName(), newPrimaryKeyColumnDto.getColumnName(), e.getMessage());
            throw e;
        }
    }

    @DeleteMapping("/{schemaName}/{tableName}/{columnName}")
    public ResponseEntity<ApiResponse<Void>> deleteColumn(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @PathVariable String columnName,
            @RequestParam(defaultValue = "false") boolean force
    ) {
        try {
            boolean deleted = columnService.deleteColumn(schemaName, tableName, columnName, force);

            if (deleted) {
                auditService.auditSuccessfulAction(ActionType.DELETE_COLUMN, schemaName, tableName, columnName);

                return ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Column deleted successfully")
                        .success(true)
                        .build());
            } else {
                auditService.auditFailedAction(ActionType.DELETE_COLUMN, schemaName, tableName, columnName, "Column deletion failed");

                return ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Column has not been deleted")
                        .success(false)
                        .build());
            }
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.DELETE_COLUMN, schemaName, tableName, columnName, e.getMessage());
            throw e;
        }
    }

    @PatchMapping("/rename")
    public ResponseEntity<ApiResponse<BaseTableColumnMetadataDto>> renameColumn(
            @Valid @RequestBody RenameColumnDto renameColumnDto
    ) {
        try {
            BaseTableColumnMetadataDto updatedColumn = columnService.renameColumn(renameColumnDto);

            auditService.auditSuccessfulAction(ActionType.UPDATE_COLUMN, renameColumnDto.getSchemaName(),
                    renameColumnDto.getTableName(), renameColumnDto.getColumnName());

            return ResponseEntity.ok(ApiResponse.<BaseTableColumnMetadataDto>builder()
                    .message("Column renamed successfully")
                    .success(true)
                    .data(updatedColumn)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.UPDATE_COLUMN, renameColumnDto.getSchemaName(),
                    renameColumnDto.getTableName(), renameColumnDto.getColumnName(), e.getMessage());
            throw e;
        }
    }

    @PatchMapping("/data-type")
    public ResponseEntity<ApiResponse<BaseTableColumnMetadataDto>> updateColumnDataType(
            @Valid @RequestBody UpdateColumnDataTypeDto updateColumnDataTypeDto
    ) {
        try {
            BaseTableColumnMetadataDto updatedColumn = columnService.updateColumnDataType(updateColumnDataTypeDto);

            auditService.auditSuccessfulAction(ActionType.UPDATE_COLUMN, updatedColumn.getTable().getSchema().getSchemaName(),
                    updatedColumn.getTable().getTableName(), updatedColumn.getColumnName());

            return ResponseEntity.ok(ApiResponse.<BaseTableColumnMetadataDto>builder()
                    .message("Column data type updated successfully")
                    .success(true)
                    .data(updatedColumn)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.UPDATE_COLUMN, updateColumnDataTypeDto.getSchemaName(),
                    updateColumnDataTypeDto.getTableName(), updateColumnDataTypeDto.getColumnName(), e.getMessage());
            throw e;
        }
    }

    @PatchMapping("/auto-increment")
    public ResponseEntity<ApiResponse<BaseTableColumnMetadataDto>> updateColumnAutoIncrement(
            @Valid @RequestBody UpdateColumnAutoIncrementDto updateColumnAutoIncrementDto
    ) {
        try {
            BaseTableColumnMetadataDto updatedColumn = columnService.updateColumnAutoIncrement(updateColumnAutoIncrementDto);

            auditService.auditSuccessfulAction(ActionType.UPDATE_COLUMN, updatedColumn.getTable().getSchema().getSchemaName(),
                    updatedColumn.getTable().getTableName(), updatedColumn.getColumnName());

            return ResponseEntity.ok(ApiResponse.<BaseTableColumnMetadataDto>builder()
                    .message("Column auto increment updated successfully")
                    .success(true)
                    .data(updatedColumn)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.UPDATE_COLUMN, updateColumnAutoIncrementDto.getSchemaName(),
                    updateColumnAutoIncrementDto.getTableName(), updateColumnAutoIncrementDto.getColumnName(), e.getMessage());
            throw e;
        }
    }

    @PatchMapping("/nullable")
    public ResponseEntity<ApiResponse<BaseTableColumnMetadataDto>> updateColumnNullable(
            @Valid @RequestBody UpdateColumnNullableDto updateColumnNullableDto,
            @RequestParam(defaultValue = "false") boolean populate
    ) {
        try {
            BaseTableColumnMetadataDto updatedColumn = columnService.updateColumnNullable(updateColumnNullableDto, populate);

            auditService.auditSuccessfulAction(ActionType.UPDATE_COLUMN, updatedColumn.getTable().getSchema().getSchemaName(),
                    updatedColumn.getTable().getTableName(), updatedColumn.getColumnName());

            return ResponseEntity.ok(ApiResponse.<BaseTableColumnMetadataDto>builder()
                    .message("Column nullable constraint updated successfully")
                    .success(true)
                    .data(updatedColumn)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.UPDATE_COLUMN, updateColumnNullableDto.getSchemaName(),
                    updateColumnNullableDto.getTableName(), updateColumnNullableDto.getColumnName(), e.getMessage());
            throw e;
        }
    }

    @PatchMapping("/unique")
    public ResponseEntity<ApiResponse<BaseTableColumnMetadataDto>> updateColumnUnique(
            @Valid @RequestBody UpdateColumnUniqueDto updateColumnUniqueDto
    ) {
        try {
            BaseTableColumnMetadataDto updatedColumn = columnService.updateColumnUnique(updateColumnUniqueDto);

            auditService.auditSuccessfulAction(ActionType.UPDATE_COLUMN, updatedColumn.getTable().getSchema().getSchemaName(),
                    updatedColumn.getTable().getTableName(), updatedColumn.getColumnName());

            return ResponseEntity.ok(ApiResponse.<BaseTableColumnMetadataDto>builder()
                    .message("Column unique constraint updated successfully")
                    .success(true)
                    .data(updatedColumn)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.UPDATE_COLUMN, updateColumnUniqueDto.getSchemaName(),
                    updateColumnUniqueDto.getTableName(), updateColumnUniqueDto.getColumnName(), e.getMessage());
            throw e;
        }
    }

    @PatchMapping("/default")
    public ResponseEntity<ApiResponse<BaseTableColumnMetadataDto>> updateColumnDefault(
            @Valid @RequestBody UpdateColumnDefaultDto updateColumnDefaultDto
    ) {
        try {
            BaseTableColumnMetadataDto updatedColumn = columnService.updateColumnDefault(updateColumnDefaultDto);

            auditService.auditSuccessfulAction(ActionType.UPDATE_COLUMN, updatedColumn.getTable().getSchema().getSchemaName(),
                    updatedColumn.getTable().getTableName(), updatedColumn.getColumnName());

            return ResponseEntity.ok(ApiResponse.<BaseTableColumnMetadataDto>builder()
                    .message("Column default value updated successfully")
                    .success(true)
                    .data(updatedColumn)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.UPDATE_COLUMN, updateColumnDefaultDto.getSchemaName(),
                    updateColumnDefaultDto.getTableName(), updateColumnDefaultDto.getColumnName(), e.getMessage());
            throw e;
        }
    }

    @PatchMapping("/primary-key")
    public ResponseEntity<ApiResponse<List<BaseTableColumnMetadataDto>>> updateColumnPrimaryKey(
            @Valid @RequestBody UpdateColumnPrimaryKeyDto updateColumnPrimaryKeyDto,
            @RequestParam(defaultValue = "false") boolean force
    ) {
        try {
            List<BaseTableColumnMetadataDto> updatedColumns = columnService.updateColumnPrimaryKey(updateColumnPrimaryKeyDto, force);

            auditService.auditFailedAction(
                    ActionType.UPDATE_COLUMN,
                    updatedColumns.get(0).getTable().getSchema().getSchemaName(),
                    updatedColumns.get(0).getTable().getTableName(),
                    String.join("|", updatedColumns.stream()
                            .map(BaseTableColumnMetadataDto::getColumnName).toList()));

            return ResponseEntity.ok(ApiResponse.<List<BaseTableColumnMetadataDto>>builder()
                    .message("Column primary key constraint updated successfully")
                    .success(true)
                    .data(updatedColumns)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(
                    ActionType.UPDATE_COLUMN,
                    updateColumnPrimaryKeyDto.getSchemaName(),
                    updateColumnPrimaryKeyDto.getTableName(),
                    String.join("|", updateColumnPrimaryKeyDto.getColumnNames()),
                    e.getMessage());
            throw e;
        }
    }

    @PatchMapping("/foreign-key")
    public ResponseEntity<ApiResponse<BaseTableColumnMetadataDto>> updateColumnForeignKey(
            @Valid @RequestBody UpdateColumnForeignKeyDto updateColumnForeignKeyDto
    ) {
        try {
            BaseTableColumnMetadataDto updatedColumn = columnService.updateColumnForeignKey(updateColumnForeignKeyDto);

            auditService.auditSuccessfulAction(ActionType.UPDATE_COLUMN, updatedColumn.getTable().getSchema().getSchemaName(),
                    updatedColumn.getTable().getTableName(), updatedColumn.getColumnName());

            return ResponseEntity.ok(ApiResponse.<BaseTableColumnMetadataDto>builder()
                    .message("Column foreign key constraint updated successfully")
                    .success(true)
                    .data(updatedColumn)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.UPDATE_COLUMN, updateColumnForeignKeyDto.getSchemaName(),
                    updateColumnForeignKeyDto.getTableName(), updateColumnForeignKeyDto.getColumnName(), e.getMessage());
            throw e;
        }
    }
}
