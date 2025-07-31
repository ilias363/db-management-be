package ma.ilias.dbmanagementbe.dao.repositories;

import ma.ilias.dbmanagementbe.dao.entities.AppUser;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;

public interface AppUserRepository extends JpaRepository<AppUser, Long> {
    Optional<AppUser> findByUsername(String username);

    Page<AppUser> findByUsernameContainingIgnoreCase(String username, Pageable pageable);

    Page<AppUser> findByActiveAndUsernameContainingIgnoreCase(Boolean active, String username, Pageable pageable);

    Page<AppUser> findByActive(Boolean active, Pageable pageable);

    @Query("SELECT COUNT(u) > 0 FROM AppUser u JOIN u.roles r WHERE r.id = :roleId")
    boolean existsByRolesId(@Param("roleId") Long roleId);

    @Query("SELECT COUNT(u) FROM AppUser u WHERE u.active = true")
    long countActiveUsers();

    @Query("SELECT COUNT(u) FROM AppUser u JOIN u.roles r WHERE r.name = 'ADMIN'")
    long countAdminUsers();

    @Query("SELECT COUNT(u) FROM AppUser u " +
            "WHERE YEAR(u.createdAt) = YEAR(CURRENT_DATE) " +
            "AND MONTH(u.createdAt) = MONTH(CURRENT_DATE)")
    long countUsersCreatedThisMonth();
}
