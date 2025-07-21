package ma.ilias.dbmanagementbe.metadata.service.schema;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.metadata.dto.schema.NewSchemaDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.table.TableService;
import ma.ilias.dbmanagementbe.util.SqlSecurityUtils;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Service
@AllArgsConstructor
@Transactional
public class MySqlSchemaManager implements SchemaService {

    private final JdbcTemplate jdbcTemplate;
    private final TableService tableService;

    @Override
    public Boolean schemaExists(String schemaName) {
        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(schemaName);

        String schemaSql = "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA WHERE SCHEMA_NAME = ?";

        List<String> schemas = jdbcTemplate.query(
                schemaSql,
                ps -> ps.setString(1, validatedSchemaName),
                (rs, rowNum) -> rs.getString("SCHEMA_NAME")
        );

        return !schemas.isEmpty();
    }

    @Override
    public Boolean isSystemSchemaByName(String schemaName) {
        return List.of("mysql", "sys", "information_schema", "performance_schema")
                .contains(schemaName.trim().toLowerCase());
    }

    @Override
    public SchemaMetadataDto getSchemaByName(String schemaName, boolean includeTables, boolean checkSchemaExists) {
        if (checkSchemaExists && !schemaExists(schemaName)) {
            throw new SchemaNotFoundException(schemaName);
        }

        return SchemaMetadataDto.builder()
                .schemaName(schemaName.toLowerCase())
                .isSystemSchema(isSystemSchemaByName(schemaName))
                .creationDate(null)
                .tables(includeTables ?
                        tableService.getTablesBySchema(schemaName, false, false, false, false)
                        : null)
                .build();
    }

    @Override
    public List<SchemaMetadataDto> getAllSchemas(Boolean includeSystemSchemas) {
        String schemaSql = "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA";

        return jdbcTemplate.query(schemaSql, (rs) -> {
            List<SchemaMetadataDto> result = new ArrayList<>();

            while (rs.next()) {
                String schemaName = rs.getString("SCHEMA_NAME");

                if (isSystemSchemaByName(schemaName) && !includeSystemSchemas) {
                    continue;
                }

                result.add(getSchemaByName(schemaName, true, false));
            }

            return result;
        });
    }

    @Override
    public SchemaMetadataDto createSchema(NewSchemaDto newSchema) {
        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(newSchema.getSchemaName());

        jdbcTemplate.execute("CREATE DATABASE " + validatedSchemaName);

        return getSchemaByName(newSchema.getSchemaName(), false, false);
    }

    @Override
    public Boolean deleteSchema(String schemaName) {
        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(schemaName);

        if (isSystemSchemaByName(schemaName)) {
            throw new UnauthorizedActionException("Cannot delete system schema: " + schemaName);
        }

        if (!schemaExists(schemaName)) {
            throw new SchemaNotFoundException(schemaName);
        }

        jdbcTemplate.execute("DROP DATABASE " + validatedSchemaName);
        return !schemaExists(schemaName);
    }
}
