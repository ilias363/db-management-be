package ma.ilias.dbmanagementbe.record.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class FilterCriteriaDto {
    @NotBlank(message = "Column name is required")
    private String columnName;

    @NotNull(message = "Operator is required")
    private FilterOperator operator;

    private Object value;
    private List<Object> values; // For IN, NOT_IN operators

    // For range queries (BETWEEN)
    private Object minValue;
    private Object maxValue;

    // Case sensitivity for string operations
    @Builder.Default
    private boolean caseSensitive = false;

    public enum FilterOperator {
        // Equality
        EQUALS,
        NOT_EQUALS,

        // Null checks
        IS_NULL,
        IS_NOT_NULL,

        // Numeric/Date comparisons
        GREATER_THAN,
        GREATER_THAN_OR_EQUAL,
        LESS_THAN,
        LESS_THAN_OR_EQUAL,
        BETWEEN,

        // String operations
        LIKE,
        NOT_LIKE,
        STARTS_WITH,
        ENDS_WITH,
        CONTAINS,

        // List operations
        IN,
        NOT_IN
    }
}
