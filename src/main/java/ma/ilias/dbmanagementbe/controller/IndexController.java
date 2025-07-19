package ma.ilias.dbmanagementbe.controller;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.metadata.dto.index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.index.IndexService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

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
}
