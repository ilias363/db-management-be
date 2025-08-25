package ma.ilias.dbmanagementbe.analytics.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class UserDatabaseAccessDto {
    private String schemaName;
    private long accessCount;
    private LocalDateTime lastAccessed;
}
