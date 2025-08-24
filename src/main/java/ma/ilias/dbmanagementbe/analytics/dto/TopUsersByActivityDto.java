package ma.ilias.dbmanagementbe.analytics.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TopUsersByActivityDto {
    private String username;
    private Long actionCount;
    private String lastActivity; // TIMESTAMP or unknown
}
