package ma.ilias.dbmanagementbe.dto.sql;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class SqlExecutionRequest {
    private String sql; // Raw SQL entered by admin
    private Integer maxRows = 500; // Safety limit
}
