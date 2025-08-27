package ma.ilias.dbmanagementbe.export.service;

import jakarta.servlet.http.HttpServletResponse;
import ma.ilias.dbmanagementbe.enums.ExportFormat;
import ma.ilias.dbmanagementbe.export.dto.ExportJobDto;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import java.io.IOException;

public interface ExportService {
    StreamingResponseBody exportUsers(ExportFormat format, HttpServletResponse response) throws IOException;

    StreamingResponseBody exportRoles(ExportFormat format, HttpServletResponse response) throws IOException;

    StreamingResponseBody exportAudits(ExportFormat format, HttpServletResponse response) throws IOException;

    ExportJobDto createAsyncExport(String resource, ExportFormat format);

    ExportJobDto getJob(String jobId);

    byte[] getJobFile(String jobId);
}
