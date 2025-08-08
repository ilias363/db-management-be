package ma.ilias.dbmanagementbe.controller;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Pattern;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.metadata.dto.view.UpdateViewDto;
import ma.ilias.dbmanagementbe.metadata.dto.view.ViewListResponseDto;
import ma.ilias.dbmanagementbe.metadata.dto.view.ViewMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.view.ViewService;
import ma.ilias.dbmanagementbe.record.dto.ViewRecordPageDto;
import ma.ilias.dbmanagementbe.record.service.RecordService;
import ma.ilias.dbmanagementbe.service.AuditService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/views")
@AllArgsConstructor
public class ViewController {

    private final ViewService viewService;
    private final AuditService auditService;
    private final RecordService recordService;

    @GetMapping("/{schemaName}")
    public ResponseEntity<ApiResponse<ViewListResponseDto>> getAllViewsInSchema(@PathVariable String schemaName) {

        List<ViewMetadataDto> views = viewService.getViewsBySchema(schemaName, false, false, true);
        return ResponseEntity.ok(ApiResponse.<ViewListResponseDto>builder()
                .message("Views fetched successfully")
                .success(true)
                .data(new ViewListResponseDto(views))
                .build());
    }

    @GetMapping("/{schemaName}/{viewName}")
    public ResponseEntity<ApiResponse<ViewMetadataDto>> getView(
            @PathVariable String schemaName,
            @PathVariable String viewName) {

        ViewMetadataDto view = viewService.getView(schemaName, viewName, true, true, true, true);
        return ResponseEntity.ok(ApiResponse.<ViewMetadataDto>builder()
                .message("View fetched successfully")
                .success(true)
                .data(view)
                .build());
    }

    @GetMapping("/{schemaName}/{viewName}/records")
    public ResponseEntity<ApiResponse<ViewRecordPageDto>> getViewRecords(
            @PathVariable String schemaName,
            @PathVariable String viewName,
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) @Max(100) int size,
            @RequestParam(required = false) String sortBy,
            @RequestParam(defaultValue = "ASC") @Pattern(regexp = "^(ASC|DESC)$",
                    message = "Sort direction must be either ASC or DESC") String sortDirection
    ) {
        ViewRecordPageDto records = recordService.getViewRecords(schemaName, viewName, page, size, sortBy, sortDirection);
        return ResponseEntity.ok(ApiResponse.<ViewRecordPageDto>builder()
                .message("View records fetched successfully")
                .success(true)
                .data(records)
                .build());
    }

    @PutMapping
    public ResponseEntity<ApiResponse<ViewMetadataDto>> renameView(
            @Valid @RequestBody UpdateViewDto updateViewDto
    ) {
        try {
            ViewMetadataDto updatedView = viewService.renameView(updateViewDto);

            auditService.auditSuccessfulAction(ActionType.UPDATE_VIEW, updateViewDto.getSchemaName(),
                    updateViewDto.getViewName(), null);

            return ResponseEntity.ok(ApiResponse.<ViewMetadataDto>builder()
                    .message("View updated successfully")
                    .success(true)
                    .data(updatedView)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.UPDATE_VIEW, updateViewDto.getSchemaName(),
                    updateViewDto.getViewName(), null, e.getMessage());
            throw e;
        }
    }

    @DeleteMapping("/{schemaName}/{viewName}")
    public ResponseEntity<ApiResponse<Void>> deleteView(
            @PathVariable String schemaName,
            @PathVariable String viewName) {
        try {
            boolean deleted = viewService.deleteView(schemaName, viewName);

            if (deleted) {
                auditService.auditSuccessfulAction(ActionType.DELETE_VIEW, schemaName, viewName);

                return ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("View deleted successfully")
                        .success(true)
                        .build());
            } else {
                auditService.auditFailedAction(ActionType.DELETE_VIEW, schemaName, viewName, "View deletion failed");

                return ResponseEntity.ok(ApiResponse.<Void>builder()
                        .message("View has not been deleted")
                        .success(false)
                        .build());
            }
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.DELETE_VIEW, schemaName, viewName, e.getMessage());
            throw e;
        }
    }
}
