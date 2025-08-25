package ma.ilias.dbmanagementbe.dao.repositories;

import ma.ilias.dbmanagementbe.dao.entities.Role;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface RoleRepository extends JpaRepository<Role, Long> {
    Optional<Role> findByName(String roleName);

    @Query("SELECT r FROM Role r WHERE " +
            "(:search IS NULL OR :search = '' OR " +
            "LOWER(r.name) LIKE LOWER(CONCAT('%', :search, '%')) OR " +
            "LOWER(r.description) LIKE LOWER(CONCAT('%', :search, '%')))")
    Page<Role> findAllWithSearch(@Param("search") String search, Pageable pageable);

    @Query("SELECT COUNT(r) FROM Role r WHERE r.isSystemRole = true")
    long countSystemRoles();

    @Query("SELECT COUNT(r) FROM AppUser u JOIN u.roles r")
    long countRoleAssignments();

    @Query("""
            SELECT r.name, COUNT(u)
            FROM AppUser u
            JOIN u.roles r
            GROUP BY r.name
            ORDER BY COUNT(u) DESC
            """)
    List<Object[]> findRoleDistribution();
}
