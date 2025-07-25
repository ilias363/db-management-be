package ma.ilias.dbmanagementbe.dto.auditlog;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class AuditLogPageDto {
    private List<AuditLogDto> audits;
    private long totalAudits;
    private int currentPage;
    private int pageSize;
    private int totalPages;
}
