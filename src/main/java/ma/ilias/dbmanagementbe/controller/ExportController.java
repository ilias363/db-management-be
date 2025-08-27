package ma.ilias.dbmanagementbe.controller;

import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.enums.ExportFormat;
import ma.ilias.dbmanagementbe.export.dto.ExportJobDto;
import ma.ilias.dbmanagementbe.export.service.ExportService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import java.io.IOException;

@RestController
@RequestMapping("/api/export")
@RequiredArgsConstructor
public class ExportController {

    private final ExportService exportService;

    @GetMapping("/users")
    public StreamingResponseBody exportUsers(@RequestParam(required = false) String format, HttpServletResponse response) throws IOException {
        return exportService.exportUsers(ExportFormat.fromParam(format), response);
    }

    @GetMapping("/roles")
    public StreamingResponseBody exportRoles(@RequestParam(required = false) String format, HttpServletResponse response) throws IOException {
        return exportService.exportRoles(ExportFormat.fromParam(format), response);
    }

    @GetMapping("/audits")
    public StreamingResponseBody exportAudits(@RequestParam(required = false) String format, HttpServletResponse response) throws IOException {
        return exportService.exportAudits(ExportFormat.fromParam(format), response);
    }

    @PostMapping("/jobs")
    public ResponseEntity<ApiResponse<ExportJobDto>> createJob(@RequestParam String resource, @RequestParam(required = false) String format) {
        ExportJobDto job = exportService.createAsyncExport(resource, ExportFormat.fromParam(format));
        return new ResponseEntity<>(ApiResponse.<ExportJobDto>builder().success(true).message("Export job created").data(job).build(), HttpStatus.ACCEPTED);
    }

    @GetMapping("/jobs/{jobId}")
    public ResponseEntity<ApiResponse<ExportJobDto>> getJob(@PathVariable String jobId) {
        ExportJobDto job = exportService.getJob(jobId);
        if (job == null)
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(ApiResponse.<ExportJobDto>builder().success(false).message("Job not found").build());
        return ResponseEntity.ok(ApiResponse.<ExportJobDto>builder().success(true).message("Job fetched").data(job).build());
    }

    @GetMapping("/jobs/{jobId}/download")
    public void downloadJob(@PathVariable String jobId, HttpServletResponse response) throws IOException {
        ExportJobDto job = exportService.getJob(jobId);
        if (job == null || job.getDownloadUrl() == null) {
            response.setStatus(HttpStatus.NOT_FOUND.value());
            return;
        }
        byte[] file = exportService.getJobFile(jobId);
        String filename = job.getResource() + "_job_" + job.getId() + (job.getFormat() == ExportFormat.CSV ? ".csv" : ".json");
        response.setHeader("Content-Disposition", "attachment; filename=" + filename);
        response.setContentType(job.getFormat() == ExportFormat.CSV ? "text/csv" : "application/json");
        response.getOutputStream().write(file);
    }
}
