package ma.ilias.dbmanagementbe.controller;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/columns")
@AllArgsConstructor
public class ColumnController {

    private final ColumnService columnService;

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
}
