package ma.ilias.dbmanagementbe.dto.auditlog;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.dto.appuser.AppUserDto;
import ma.ilias.dbmanagementbe.enums.ActionType;

import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class AuditLogDto {
    private Long id;
    private AppUserDto user;
    private ActionType actionType;
    private String schemaName;
    private String tableName;
    private String objectName;
    private String actionDetails;
    private Boolean successful;
    private String errorMessage;
    private LocalDateTime auditTimestamp;
}
