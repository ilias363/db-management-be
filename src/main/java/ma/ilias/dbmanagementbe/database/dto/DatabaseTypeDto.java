package ma.ilias.dbmanagementbe.database.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class DatabaseTypeDto {
    private String type;
    private String displayName;
}
