package ma.ilias.dbmanagementbe.analytics.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class DatabaseTypeDto {
    private String type;
    private String displayName;
}
