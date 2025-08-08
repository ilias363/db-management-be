package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.metadata.dto.schema.NewSchemaDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaListResponseDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaService;
import ma.ilias.dbmanagementbe.service.AuditService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/schemas")
@AllArgsConstructor
public class SchemaController {

    private final SchemaService schemaService;
    private final AuditService auditService;

    @PostMapping
    public ResponseEntity<ApiResponse<SchemaMetadataDto>> createSchema(@Valid @RequestBody NewSchemaDto newSchemaDto) {
        try {
            SchemaMetadataDto createdSchema = schemaService.createSchema(newSchemaDto);

            auditService.auditSuccessfulAction(ActionType.CREATE_SCHEMA, newSchemaDto.getSchemaName(), null);

            return new ResponseEntity<>(ApiResponse.<SchemaMetadataDto>builder()
                    .message("Schema created successfully")
                    .success(true)
                    .data(createdSchema)
                    .build(), HttpStatus.CREATED);
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.CREATE_SCHEMA, newSchemaDto.getSchemaName(), null, e.getMessage());
            throw e;
        }
    }

    @GetMapping("/{schemaName}")
    public ResponseEntity<ApiResponse<SchemaMetadataDto>> getSchemaByName(@PathVariable String schemaName) {
        SchemaMetadataDto schema = schemaService.getSchemaByName(
                schemaName, true, true, true, true);
        return ResponseEntity.ok(ApiResponse.<SchemaMetadataDto>builder()
                .message("Schema fetched successfully")
                .success(true)
                .data(schema)
                .build());
    }

    @GetMapping
    public ResponseEntity<ApiResponse<SchemaListResponseDto>> getAllSchemas(
            @RequestParam(defaultValue = "false") boolean includeSystem
    ) {
        List<SchemaMetadataDto> schemas = schemaService.getAllSchemas(includeSystem);
        return ResponseEntity.ok(ApiResponse.<SchemaListResponseDto>builder()
                .message("Schemas fetched successfully")
                .success(true)
                .data(new SchemaListResponseDto(schemas))
                .build());
    }

    @DeleteMapping("/{schemaName}")
    public ResponseEntity<ApiResponse<Void>> deleteSchema(@PathVariable String schemaName) {
        try {
            boolean deleted = schemaService.deleteSchema(schemaName);

            if (deleted) {
                auditService.auditSuccessfulAction(ActionType.DELETE_SCHEMA, schemaName, null);

                return ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Schema deleted successfully")
                        .success(true)
                        .build());
            } else {
                auditService.auditFailedAction(ActionType.DELETE_SCHEMA, schemaName, null, "Schema deletion failed");

                return ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("Schema has not been deleted")
                        .success(false)
                        .build());
            }
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.DELETE_SCHEMA, schemaName, null, e.getMessage());
            throw e;
        }
    }
}
