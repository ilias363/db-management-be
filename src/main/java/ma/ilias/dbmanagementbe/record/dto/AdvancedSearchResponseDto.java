package ma.ilias.dbmanagementbe.record.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class AdvancedSearchResponseDto {
    @JsonIgnoreProperties({"schemaName", "tableName"})
    private List<RecordDto> records;

    private long totalRecords;
    private long filteredRecords; // Records after filtering but before pagination
    private int currentPage;
    private int pageSize;
    private int totalPages;
    private String objectName;
    private String schemaName;

    // Search metadata
    private boolean hasFilters;
    private boolean hasGlobalSearch;
    private boolean hasSort;
    private boolean isDistinct;

    // Applied filters and sorts for reference
    private List<FilterCriteriaDto> appliedFilters;
    private List<SortCriteriaDto> appliedSorts;
    private String appliedGlobalSearch;
}
