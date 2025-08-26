package ma.ilias.dbmanagementbe.service;

import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.dto.sql.SqlExecutionRequest;
import ma.ilias.dbmanagementbe.dto.sql.SqlResultSet;
import ma.ilias.dbmanagementbe.exception.InsufficientPermissionException;
import ma.ilias.dbmanagementbe.util.AuthorizationUtils;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Service;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.time.Duration;
import java.time.Instant;
import java.util.*;

@Service
@RequiredArgsConstructor
public class SqlExecutionService {

    private final JdbcTemplate jdbcTemplate;

    private static final Set<String> READ_ONLY_STATEMENTS = Set.of(
            "SELECT", "SHOW", "DESCRIBE", "EXPLAIN"
    );

    private static final Set<String> ALLOWED_STATEMENTS = Set.of(
            "SELECT", "SHOW", "DESCRIBE", "EXPLAIN",
            "INSERT", "UPDATE", "DELETE", "CREATE", "ALTER", "DROP", "TRUNCATE", "RENAME"
    );

    public SqlResultSet execute(SqlExecutionRequest request) {
        if (request.getSql() == null || request.getSql().isBlank()) {
            throw new IllegalArgumentException("SQL cannot be empty");
        }

        // Ensure only system admin can run arbitrary SQL
        if (!AuthorizationUtils.isSystemAdmin()) {
            throw new InsufficientPermissionException("Only system administrators can execute raw SQL");
        }

        String sql = request.getSql().trim();
        String leading = sql.split("\\s+")[0].toUpperCase(Locale.ROOT);

        if (!ALLOWED_STATEMENTS.contains(leading)) {
            throw new IllegalArgumentException("Statement type not allowed: " + leading);
        }

        Instant start = Instant.now();
        SqlResultSet result;
        if (READ_ONLY_STATEMENTS.contains(leading)) {
            result = runQuery(sql, request.getMaxRows());
        } else {
            int updated = jdbcTemplate.update(sql);
            result = SqlResultSet.builder()
                    .columns(Collections.emptyList())
                    .rows(Collections.emptyList())
                    .rowCount(updated)
                    .statementType(leading)
                    .build();
        }

        result.setExecutionTimeMs(Duration.between(start, Instant.now()).toMillis());
        return result;
    }

    private SqlResultSet runQuery(String sql, Integer maxRows) {
        List<String> columns = new ArrayList<>();

        jdbcTemplate.setMaxRows(maxRows == null ? 500 : maxRows);

        RowMapper<Map<String, Object>> rowMapper = (rs, rowNum) -> mapRow(rs, columns);
        List<Map<String, Object>> queryRows = jdbcTemplate.query(sql, rowMapper);

        return SqlResultSet.builder()
                .columns(columns)
                .rows(queryRows)
                .rowCount(queryRows.size())
                .statementType("SELECT")
                .build();
    }

    private Map<String, Object> mapRow(ResultSet rs, List<String> columns) throws SQLException {
        if (columns.isEmpty()) {
            ResultSetMetaData meta = rs.getMetaData();
            for (int i = 1; i <= meta.getColumnCount(); i++) {
                columns.add(meta.getColumnLabel(i));
            }
        }
        Map<String, Object> row = new LinkedHashMap<>();
        for (String col : columns) {
            row.put(col, rs.getObject(col));
        }
        return row;
    }
}
