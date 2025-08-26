package ma.ilias.dbmanagementbe.dto.sql;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SqlResultSet {
    private List<String> columns;
    private List<Map<String, Object>> rows;
    private Integer rowCount; // number of rows returned or affected
    private Long executionTimeMs;
    private String statementType; // SELECT / UPDATE / etc
}
