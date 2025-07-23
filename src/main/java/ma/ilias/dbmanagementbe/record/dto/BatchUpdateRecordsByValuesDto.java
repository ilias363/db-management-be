package ma.ilias.dbmanagementbe.record.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class BatchUpdateRecordsByValuesDto {
    @NotBlank(message = "Schema name cannot be blank")
    private String schemaName;

    @NotBlank(message = "Table name cannot be blank")
    private String tableName;

    @NotEmpty(message = "Updates list cannot be empty")
    private List<@NotNull SingleUpdateRecordByValuesDto> updates;

    @NoArgsConstructor
    @AllArgsConstructor
    @Data
    @Builder
    public static class SingleUpdateRecordByValuesDto {
        @NotNull(message = "New data cannot be null")
        private Map<String, Object> newData;

        @NotNull(message = "Identifying values cannot be null")
        private Map<String, Object> identifyingValues;

        @Builder.Default
        private boolean allowMultiple = false; // Safety flag to allow updating multiple matching records
    }
}
