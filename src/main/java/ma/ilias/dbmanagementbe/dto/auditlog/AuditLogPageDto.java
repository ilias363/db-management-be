package ma.ilias.dbmanagementbe.dto.auditlog;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Collections;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class AuditLogPageDto {
    @Builder.Default
    private List<AuditLogDto> audits = Collections.emptyList();
    private long totalAudits;
    private int currentPage;
    private int pageSize;
    private int totalPages;
}
