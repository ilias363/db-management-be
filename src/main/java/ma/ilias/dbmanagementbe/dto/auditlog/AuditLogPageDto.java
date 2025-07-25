package ma.ilias.dbmanagementbe.dto.auditlog;

import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.dto.PageDto;

@SuperBuilder
@NoArgsConstructor
public class AuditLogPageDto extends PageDto<AuditLogDto> {
}
