package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.metadata.dto.index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.NewIndexDto;
import ma.ilias.dbmanagementbe.metadata.service.index.IndexService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/indexes")
@AllArgsConstructor
public class IndexController {

    private final IndexService indexService;

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
        IndexMetadataDto createdIndex = indexService.createIndex(newIndexDto);
        return new ResponseEntity<>(ApiResponse.<IndexMetadataDto>builder()
                .message("Index created successfully")
                .success(true)
                .data(createdIndex)
                .build(), HttpStatus.CREATED);
    }

    @DeleteMapping("/{schemaName}/{tableName}/{indexName}")
    public ResponseEntity<ApiResponse<Void>> deleteIndex(
            @PathVariable String schemaName,
            @PathVariable String tableName,
            @PathVariable String indexName) {
        boolean deleted = indexService.deleteIndex(schemaName, tableName, indexName);
        if (deleted) {
            return ResponseEntity.ok(ApiResponse.<Void>builder()
                    .message("Index deleted successfully")
                    .success(true)
                    .build());
        } else {
            return ResponseEntity.ok(ApiResponse.<Void>builder()
                    .message("Index has not been deleted")
                    .success(false)
                    .build());
        }
    }
}
