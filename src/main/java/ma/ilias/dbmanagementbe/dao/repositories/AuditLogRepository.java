package ma.ilias.dbmanagementbe.dao.repositories;

import ma.ilias.dbmanagementbe.dao.entities.AuditLog;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface AuditLogRepository extends JpaRepository<AuditLog, Long> {
    List<AuditLog> findByUser_Id(Long userId);
}
