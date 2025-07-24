package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.metadata.dto.index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.NewIndexDto;
import ma.ilias.dbmanagementbe.metadata.service.index.IndexService;
import ma.ilias.dbmanagementbe.service.AuditService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/indexes")
@AllArgsConstructor
public class IndexController {

    private final IndexService indexService;
    private final AuditService auditService;

    @GetMapping("/{schemaName}/{tableName}/{indexName}")
    public ResponseEntity<ApiResponse<IndexMetadataDto>> getIndex(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @PathVariable String indexName) {
        IndexMetadataDto index = indexService.getIndex(schemaName, tableName, indexName, true, true);
        return ResponseEntity.ok(ApiResponse.<IndexMetadataDto>builder()
                .message("Index fetched successfully")
                .success(true)
                .data(index)
                .build());
    }

    @GetMapping("/{schemaName}/{tableName}")
    public ResponseEntity<ApiResponse<List<IndexMetadataDto>>> getIndexesByTable(
            @PathVariable String schemaName,
            @PathVariable String tableName) {
        List<IndexMetadataDto> indexes = indexService.getIndexesByTable(schemaName, tableName, true, true);
        return ResponseEntity.ok(ApiResponse.<List<IndexMetadataDto>>builder()
                .message("Indexes fetched successfully")
                .success(true)
                .data(indexes)
                .build());
    }

    @PostMapping
    public ResponseEntity<ApiResponse<IndexMetadataDto>> createIndex(
            @Valid @RequestBody NewIndexDto newIndexDto) {
        try {
            IndexMetadataDto createdIndex = indexService.createIndex(newIndexDto);

            auditService.auditSuccessfulAction(ActionType.CREATE_INDEX, createdIndex.getTable().getSchema().getSchemaName(),
                    createdIndex.getTable().getTableName(), createdIndex.getIndexName());

            return new ResponseEntity<>(ApiResponse.<IndexMetadataDto>builder()
                    .message("Index created successfully")
                    .success(true)
                    .data(createdIndex)
                    .build(), HttpStatus.CREATED);
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.CREATE_INDEX, newIndexDto.getSchemaName(),
                    newIndexDto.getTableName(), newIndexDto.getIndexName(), e.getMessage());
            throw e;
        }
    }

    @DeleteMapping("/{schemaName}/{tableName}/{indexName}")
    public ResponseEntity<ApiResponse<Void>> deleteIndex(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @PathVariable String indexName) {
        try {
            boolean deleted = indexService.deleteIndex(schemaName, tableName, indexName);

            if (deleted) {
                auditService.auditSuccessfulAction(ActionType.DELETE_INDEX, schemaName, tableName, indexName);

                return ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Index deleted successfully")
                        .success(true)
                        .build());
            } else {
                auditService.auditFailedAction(ActionType.DELETE_INDEX, schemaName, tableName, indexName, "Index deletion failed");

                return ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Index has not been deleted")
                        .success(false)
                        .build());
            }
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.DELETE_INDEX, schemaName, tableName, indexName, e.getMessage());
            throw e;
        }
    }
}
