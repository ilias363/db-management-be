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
public class BatchUpdateRecordsDto {
    @NotBlank(message = "Schema name cannot be blank")
    private String schemaName;

    @NotBlank(message = "Table name cannot be blank")
    private String tableName;

    @NotEmpty(message = "Updates list cannot be empty")
    private List<@NotNull SingleUpdateRecordDto> updates;

    @NoArgsConstructor
    @AllArgsConstructor
    @Data
    @Builder
    public static class SingleUpdateRecordDto {
        @NotNull(message = "Record data cannot be null")
        private Map<String, Object> data;

        @NotNull(message = "Primary key values cannot be null")
        private Map<String, Object> primaryKeyValues;
    }
}
