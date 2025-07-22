package ma.ilias.dbmanagementbe.record.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class NewRecordDto {
    @NotNull(message = "Record data cannot be null")
    private Map<String, Object> data;

    @NotBlank(message = "Table name cannot be blank")
    private String tableName;

    @NotBlank(message = "Schema name cannot be blank")
    private String schemaName;
}
