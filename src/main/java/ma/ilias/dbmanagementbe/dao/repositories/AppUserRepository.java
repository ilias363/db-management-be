package ma.ilias.dbmanagementbe.dao.repositories;

import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface AppUserRepository extends JpaRepository<AppUser, Long> {
    Optional<AppUser> findByUsername(String username);
    List<AppUser> findByActive(Boolean active);
    @Query("SELECT COUNT(u) > 0 FROM AppUser u JOIN u.roles r WHERE r.id = :roleId")
    boolean existsByRolesId(@Param("roleId") Long roleId);
}
