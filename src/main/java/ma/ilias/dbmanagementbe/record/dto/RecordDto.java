package ma.ilias.dbmanagementbe.record.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class RecordDto {
    private Map<String, Object> data;
    private String tableName;
    private String schemaName;
}
