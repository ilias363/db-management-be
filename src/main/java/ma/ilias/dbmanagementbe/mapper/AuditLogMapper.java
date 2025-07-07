package ma.ilias.dbmanagementbe.mapper;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dao.entities.AuditLog;
import ma.ilias.dbmanagementbe.dto.auditlog.AuditLogDto;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class AuditLogMapper {
    private final ModelMapper modelMapper;

    public AuditLogDto toDto(AuditLog auditLog) {
        return modelMapper.map(auditLog, AuditLogDto.class);
    }

    public AuditLog toEntity(AuditLogDto auditLogDto) {
        return modelMapper.map(auditLogDto, AuditLog.class);
    }
}
