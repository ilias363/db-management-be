package ma.ilias.dbmanagementbe.record.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.enums.FilterOperator;
import ma.ilias.dbmanagementbe.validation.annotations.ValidFilterOperator;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class FilterCriteriaDto {
    @NotBlank(message = "Column name is required")
    private String columnName;

    @NotNull(message = "Operator is required")
    @ValidFilterOperator
    private String operator;

    private Object value;
    private List<Object> values; // For IN, NOT_IN operators

    // For range queries (BETWEEN)
    private Object minValue;
    private Object maxValue;

    // Case sensitivity for string operations
    @Builder.Default
    private boolean caseSensitive = false;

    public FilterOperator getOperator() {
        return FilterOperator.valueOf(operator.toUpperCase());
    }
}
