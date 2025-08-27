package ma.ilias.dbmanagementbe.export.service;

import jakarta.annotation.PostConstruct;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ma.ilias.dbmanagementbe.dao.entities.Permission;
import ma.ilias.dbmanagementbe.dao.entities.Role;
import ma.ilias.dbmanagementbe.dao.repositories.AppUserRepository;
import ma.ilias.dbmanagementbe.dao.repositories.AuditLogRepository;
import ma.ilias.dbmanagementbe.dao.repositories.RoleRepository;
import ma.ilias.dbmanagementbe.enums.ActionType;
import ma.ilias.dbmanagementbe.enums.ExportFormat;
import ma.ilias.dbmanagementbe.enums.ExportJobStatus;
import ma.ilias.dbmanagementbe.export.dto.ExportJobDto;
import ma.ilias.dbmanagementbe.export.writers.ExportWriters;
import ma.ilias.dbmanagementbe.service.AuditService;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.Iterator;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

@Service
@RequiredArgsConstructor
@Slf4j
public class ExportManager implements ExportService {

    private final AppUserRepository userRepository;
    private final RoleRepository roleRepository;
    private final AuditLogRepository auditLogRepository;
    private final AuditService auditService;

    private ThreadPoolTaskExecutor executor;

    private final Map<String, ExportJobDto> jobs = new ConcurrentHashMap<>();
    private final Map<String, byte[]> jobFiles = new ConcurrentHashMap<>();

    @PostConstruct
    void init() {
        executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(2);
        executor.setMaxPoolSize(4);
        executor.setQueueCapacity(20);
        executor.setThreadNamePrefix("export-");
        executor.initialize();
    }

    @Override
    @Transactional
    public StreamingResponseBody exportUsers(ExportFormat format, HttpServletResponse response) {
        return prepareStreamingResponse("users", format, response, this::getUsersIterator);
    }

    @Override
    @Transactional
    public StreamingResponseBody exportRoles(ExportFormat format, HttpServletResponse response) {
        return prepareStreamingResponse("roles", format, response, this::getRolesIterator);
    }

    @Override
    @Transactional
    public StreamingResponseBody exportAudits(ExportFormat format, HttpServletResponse response) {
        return prepareStreamingResponse("audits", format, response, this::getAuditsIterator);
    }

    @Override
    public ExportJobDto createAsyncExport(String resource, ExportFormat format) {
        String id = UUID.randomUUID().toString();
        ExportJobDto job = ExportJobDto.builder()
                .id(id)
                .resource(resource)
                .format(format)
                .status(ExportJobStatus.PENDING)
                .createdAt(LocalDateTime.now())
                .build();
        jobs.put(id, job);

        executor.submit(() -> runJob(id));
        return job;
    }

    @Override
    public ExportJobDto getJob(String jobId) {
        return jobs.get(jobId);
    }

    @Override
    public byte[] getJobFile(String jobId) {
        return jobFiles.get(jobId);
    }

    private void runJob(String jobId) {
        ExportJobDto job = jobs.get(jobId);
        if (job == null) {
            log.warn("Export job {} not found", jobId);
            return;
        }

        log.info("Starting export job: {} for resource: {}", jobId, job.getResource());
        job.setStatus(ExportJobStatus.RUNNING);

        try (ByteArrayOutputStream baos = new ByteArrayOutputStream();
             OutputStreamWriter writer = new OutputStreamWriter(baos, StandardCharsets.UTF_8)) {

            String resource = job.getResource();
            var writerStrategy = ExportWriters.forFormat(job.getFormat());

            Iterator<Map<String, Object>> rowsIterator;
            long count;

            switch (resource) {
                case "users" -> {
                    rowsIterator = getUsersIterator();
                    count = userRepository.count();
                }
                case "roles" -> {
                    rowsIterator = getRolesIterator();
                    count = roleRepository.count();
                }
                case "audits" -> {
                    rowsIterator = getAuditsIterator();
                    count = auditLogRepository.count();
                }
                default -> {
                    throw new IllegalArgumentException("Unsupported resource: " + resource);
                }
            }

            if (count == 0) {
                log.warn("Export job {} found no data for resource: {}", jobId, resource);
            }

            writerStrategy.writeRows(writer, rowsIterator);
            writer.flush();

            byte[] data = baos.toByteArray();
            if (data.length == 0) {
                throw new RuntimeException("Export produced empty file");
            }

            jobFiles.put(jobId, data);
            job.setRecordCount(count);
            job.setStatus(ExportJobStatus.COMPLETED);
            job.setCompletedAt(LocalDateTime.now());
            job.setDownloadUrl("/api/export/jobs/" + jobId + "/download");

            log.info("Export job {} completed successfully. Records: {}, Size: {} bytes",
                    jobId, count, data.length);
            auditService.auditSuccessfulAction(ActionType.EXPORT_DATA, resource + " (" + count + " records)");

        } catch (Exception e) {
            log.error("Export job {} failed for resource: {}", jobId, job.getResource(), e);
            job.setStatus(ExportJobStatus.FAILED);
            job.setErrorMessage(e.getMessage());
            auditService.auditFailedAction(ActionType.EXPORT_DATA, job.getResource(), e.getMessage());
        }
    }

    private StreamingResponseBody prepareStreamingResponse(
            String baseName, ExportFormat format, HttpServletResponse response, Supplier<Iterator<Map<String, Object>>> supplier) {
        var writerStrategy = ExportWriters.forFormat(format);
        String filename = baseName + "_" + LocalDateTime.now().toString().replace(':', '-') + writerStrategy.fileExtension();

        log.info("Preparing streaming response for: {}, format: {}, filename: {}", baseName, format, filename);

        return outputStream -> {
            try (OutputStreamWriter osw = new OutputStreamWriter(outputStream, StandardCharsets.UTF_8)) {
                log.info("Starting to write export data for: {}", baseName);

                response.setCharacterEncoding(StandardCharsets.UTF_8.name());
                response.setContentType(writerStrategy.contentType());
                response.setHeader("Content-Disposition", "attachment; filename=" + filename);

                writerStrategy.writeRows(osw, supplier.get());
                osw.flush();

                auditService.auditSuccessfulAction(ActionType.EXPORT_DATA, baseName);
                log.info("Completed export for: {}", baseName);
            } catch (Exception e) {
                log.error("Error during export for {}: {}", baseName, e.getMessage(), e);
                auditService.auditFailedAction(ActionType.EXPORT_DATA, baseName, e.getMessage());
                throw e;
            }
        };
    }

    @Scheduled(fixedDelay = 60 * 60 * 1000)
    void cleanupOldJobs() {
        LocalDateTime cutoffCompleted = LocalDateTime.now().minusHours(1); // Keep completed jobs for 1 hour
        LocalDateTime cutoffFailed = LocalDateTime.now().minusMinutes(10); // Keep failed jobs for 10 minutes
        LocalDateTime cutoffStale = LocalDateTime.now().minusMinutes(30); // Clean up stale pending/running jobs

        jobs.entrySet().removeIf(entry -> {
            String id = entry.getKey();
            ExportJobDto job = entry.getValue();

            if (job == null) {
                jobFiles.remove(id);
                return true;
            }

            // Remove completed jobs after 1 hour
            if (job.getStatus() == ExportJobStatus.COMPLETED &&
                    job.getCompletedAt() != null &&
                    job.getCompletedAt().isBefore(cutoffCompleted)) {
                jobFiles.remove(id);
                return true;
            }

            // Remove failed jobs after 10 minutes
            if (job.getStatus() == ExportJobStatus.FAILED &&
                    job.getCreatedAt().isBefore(cutoffFailed)) {
                jobFiles.remove(id);
                return true;
            }

            // Remove stale pending/running jobs after 30 minutes
            if ((job.getStatus() == ExportJobStatus.PENDING || job.getStatus() == ExportJobStatus.RUNNING) &&
                    job.getCreatedAt().isBefore(cutoffStale)) {
                job.setStatus(ExportJobStatus.FAILED);
                job.setErrorMessage("Job timed out");
                jobFiles.remove(id);
                return true;
            }

            return false;
        });

        log.info("Cleaned up old export jobs. Active jobs: {}", jobs.size());
    }

    private Iterator<Map<String, Object>> getUsersIterator() {
        return userRepository.findAll().stream().map(u -> Map.of(
                "id", u.getId(),
                "username", u.getUsername(),
                "active", u.getActive(),
                "roles", u.getRoles().stream().map(Role::getId).toList(),
                "createdAt", u.getCreatedAt(),
                "updatedAt", u.getUpdatedAt()
        )).iterator();
    }

    private Iterator<Map<String, Object>> getRolesIterator() {
        return roleRepository.findAll().stream().map(r -> Map.of(
                "id", r.getId(),
                "name", r.getName(),
                "description", r.getDescription() != null ? r.getDescription() : "",
                "isSystemRole", r.getIsSystemRole(),
                "permissions", r.getPermissions().stream().map(Permission::getId).toList(),
                "createdAt", r.getCreatedAt(),
                "updatedAt", r.getUpdatedAt()
        )).iterator();
    }

    private Iterator<Map<String, Object>> getAuditsIterator() {
        return auditLogRepository.findAll().stream().map(a -> Map.<String, Object>of(
                "id", a.getId(),
                "username", a.getUser() != null ? a.getUser().getUsername() : "unknown",
                "actionType", a.getActionType().name(),
                "schemaName", a.getSchemaName() != null ? a.getSchemaName() : "",
                "tableName", a.getTableName() != null ? a.getTableName() : "",
                "objectName", a.getObjectName() != null ? a.getObjectName() : "",
                "actionDetails", a.getActionDetails() != null ? a.getActionDetails() : "",
                "successful", a.getSuccessful(),
                "errorMessage", a.getErrorMessage() != null ? a.getErrorMessage() : "",
                "auditTimestamp", a.getAuditTimestamp()
        )).iterator();
    }
}
