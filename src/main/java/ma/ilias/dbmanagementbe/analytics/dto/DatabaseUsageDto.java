package ma.ilias.dbmanagementbe.analytics.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DatabaseUsageDto {
    private String schemaName;
    private Long tableCount;
    private Long recordCount;
    private Long size;
    private String lastModified; // TIMESTAMP or "unknown"
}
