package ma.ilias.dbmanagementbe.export.dto;

import lombok.Builder;
import lombok.Data;
import ma.ilias.dbmanagementbe.enums.ExportFormat;
import ma.ilias.dbmanagementbe.enums.ExportJobStatus;

import java.time.LocalDateTime;

@Data
@Builder
public class ExportJobDto {
    private String id;
    private String resource; // users, roles, audits or table/view
    private ExportFormat format;
    private ExportJobStatus status;
    private String downloadUrl; // present when COMPLETED
    private String errorMessage;
    private Long recordCount;
    private LocalDateTime createdAt;
    private LocalDateTime completedAt;
}
