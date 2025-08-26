package ma.ilias.dbmanagementbe.controller;

import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.dto.sql.SqlExecutionRequest;
import ma.ilias.dbmanagementbe.dto.sql.SqlResultSet;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.service.AuditService;
import ma.ilias.dbmanagementbe.service.SqlExecutionService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/sql")
@RequiredArgsConstructor
public class SqlExecutionController {

    private final SqlExecutionService sqlExecutionService;
    private final AuditService auditService;

    @PostMapping("/execute")
    public ResponseEntity<ApiResponse<SqlResultSet>> execute(@RequestBody SqlExecutionRequest request) {
        try {
            SqlResultSet result = sqlExecutionService.execute(request);

            auditService.auditSuccessfulAction(ActionType.EXECUTE_SQL, request.getSql().trim());

            return ResponseEntity.ok(ApiResponse.<SqlResultSet>builder()
                    .message("SQL executed successfully")
                    .success(true)
                    .data(result)
                    .build());
        } catch (Exception e) {
            auditService.auditFailedAction(ActionType.EXECUTE_SQL, request.getSql().trim(), e.getMessage());
            throw e;
        }
    }
}
