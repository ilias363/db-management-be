package ma.ilias.dbmanagementbe.database.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DatabaseStatsDto {
    private long totalSchemas;
    private long totalTables;
    private long totalViews;
    private long totalRecords;
}
