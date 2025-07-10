package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.metadata.dto.schema.NewSchemaDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaMetadataService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/schemas")
@AllArgsConstructor
public class SchemaController {

    private final SchemaMetadataService schemaMetadataService;

    @PostMapping
    public ResponseEntity<ApiResponse<SchemaMetadataDto>> createSchema(@Valid @RequestBody NewSchemaDto newSchemaDto) {
        SchemaMetadataDto createdSchema = schemaMetadataService.createSchema(newSchemaDto);
        return new ResponseEntity<>(ApiResponse.<SchemaMetadataDto>builder()
                .message("Schema created successfully")
                .success(true)
                .data(createdSchema)
                .build(), HttpStatus.CREATED);
    }

    @GetMapping("/{schemaName}")
    public ResponseEntity<ApiResponse<SchemaMetadataDto>> getSchemaByName(@PathVariable String schemaName) {
        SchemaMetadataDto schema = schemaMetadataService.getSchemaByName(schemaName);
        return ResponseEntity.ok(ApiResponse.<SchemaMetadataDto>builder()
                .message("Schema fetched successfully")
                .success(true)
                .data(schema)
                .build());
    }

    @GetMapping
    public ResponseEntity<ApiResponse<List<SchemaMetadataDto>>> getAllSchemas() {
        List<SchemaMetadataDto> schemas = schemaMetadataService.getAllSchemas(false);
        return ResponseEntity.ok(ApiResponse.<List<SchemaMetadataDto>>builder()
                .message("Schemas fetched successfully")
                .success(true)
                .data(schemas)
                .build());
    }

    @GetMapping("/include-system")
    public ResponseEntity<ApiResponse<List<SchemaMetadataDto>>> getAllSchemasIncludingSystem() {
        List<SchemaMetadataDto> schemas = schemaMetadataService.getAllSchemas(true);
        return ResponseEntity.ok(ApiResponse.<List<SchemaMetadataDto>>builder()
                .message("All schemas fetched successfully")
                .success(true)
                .data(schemas)
                .build());
    }

    @DeleteMapping("/{schemaName}")
    public ResponseEntity<ApiResponse<Void>> deleteSchema(@PathVariable String schemaName) {
        return schemaMetadataService.deleteSchema(schemaName) ?
                ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Schema deleted successfully")
                        .success(true)
                        .build())
                :
                ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Schema has not been deleted")
                        .success(false)
                        .build());
    }
}
