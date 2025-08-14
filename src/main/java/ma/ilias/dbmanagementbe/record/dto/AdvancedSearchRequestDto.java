package ma.ilias.dbmanagementbe.record.dto;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class AdvancedSearchRequestDto {
    @NotBlank(message = "Schema name is required")
    private String schemaName;

    @NotBlank(message = "Table/View name is required")
    private String objectName;

    // Pagination
    @Min(value = 0, message = "Page must be >= 0")
    @Builder.Default
    private int page = 0;

    @Min(value = 1, message = "Size must be >= 1")
    @Builder.Default
    private int size = 10;

    // Sorting
    private List<@Valid SortCriteriaDto> sorts;

    // Filtering
    private List<@Valid FilterCriteriaDto> filters;

    // Global search across text columns
    private String globalSearch;

    // Distinct results
    @Builder.Default
    private boolean distinct = false;
}
