package ma.ilias.dbmanagementbe.metadata.service.schema;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.metadata.dto.schema.NewSchemaDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.PreparedStatement;
import java.util.ArrayList;
import java.util.List;

@Service
@AllArgsConstructor
@Transactional
public class MySqlSchemaManager implements SchemaMetadataService {

    private final JdbcTemplate jdbcTemplate;

    @Override
    public Boolean schemaExists(String schemaName) {
        String schemaSql = "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA WHERE SCHEMA_NAME = ?";

        List<String> schemas = jdbcTemplate.query(
                schemaSql,
                ps -> ps.setString(1, schemaName),
                (rs, rowNum) -> rs.getString("SCHEMA_NAME")
        );

        return !schemas.isEmpty();
    }

    @Override
    public Boolean isSystemSchemaByName(String schemaName) {
        return List.of("mysql", "sys", "information_schema", "performance_schema")
                .contains(schemaName.trim().toLowerCase());
    }

    private List<TableMetadataDto> queryTablesForSchema(String schemaName) {
        String tableSql = """
                SELECT t.TABLE_NAME,
                       COUNT(c.COLUMN_NAME) AS COLUMN_COUNT,
                       t.TABLE_ROWS,
                       (t.DATA_LENGTH + t.INDEX_LENGTH) AS SIZE_IN_BYTES
                FROM INFORMATION_SCHEMA.TABLES t
                LEFT JOIN INFORMATION_SCHEMA.COLUMNS c
                    ON t.TABLE_SCHEMA = c.TABLE_SCHEMA AND t.TABLE_NAME = c.TABLE_NAME
                WHERE t.TABLE_SCHEMA = ?
                GROUP BY t.TABLE_NAME, t.TABLE_ROWS, t.DATA_LENGTH, t.INDEX_LENGTH
                """;

        return jdbcTemplate.query(
                connection -> {
                    PreparedStatement ps = connection.prepareStatement(tableSql);
                    ps.setString(1, schemaName);
                    return ps;
                },
                (trs, tRowNum) -> TableMetadataDto.builder()
                        .tableName(trs.getString("TABLE_NAME"))
                        .columnCount(trs.getInt("COLUMN_COUNT"))
                        .rowCount(trs.getLong("TABLE_ROWS"))
                        .sizeInBytes(trs.getLong("SIZE_IN_BYTES"))
                        .build()
        );
    }

    @Override
    public List<SchemaMetadataDto> getAllSchemas(Boolean includeSystemSchema) {
        String schemaSql = "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA";

        return jdbcTemplate.query(schemaSql, (rs) -> {
            List<SchemaMetadataDto> result = new ArrayList<>();

            while (rs.next()) {
                String schemaName = rs.getString("SCHEMA_NAME");
                boolean isSystem = isSystemSchemaByName(schemaName);

                if (!includeSystemSchema && isSystem) {
                    continue;
                }

                List<TableMetadataDto> tables = queryTablesForSchema(schemaName);

                result.add(SchemaMetadataDto.builder()
                        .schemaName(schemaName)
                        .isSystemSchema(isSystem)
                        .creationDate(null) // MySQL doesn't provide this
                        .tables(tables)
                        .build());
            }

            return result;
        });
    }

    @Override
    public SchemaMetadataDto getSchemaByName(String schemaName) {
        if (!schemaExists(schemaName)) {
            throw new SchemaNotFoundException(schemaName);
        }

        return SchemaMetadataDto.builder()
                .schemaName(schemaName.toLowerCase())
                .isSystemSchema(isSystemSchemaByName(schemaName))
                .creationDate(null)
                .tables(queryTablesForSchema(schemaName))
                .build();
    }

    @Override
    public SchemaMetadataDto createSchema(NewSchemaDto newSchema) {
        jdbcTemplate.execute("CREATE DATABASE " + newSchema.getSchemaName().toLowerCase());

        return SchemaMetadataDto.builder()
                .schemaName(newSchema.getSchemaName().toLowerCase())
                .isSystemSchema(false)
                .creationDate(null)
                .tables(new ArrayList<>())
                .build();
    }

    @Override
    public Boolean deleteSchema(String schemaName) {
        if (isSystemSchemaByName(schemaName)) {
            throw new UnauthorizedActionException("Cannot delete system schema: " + schemaName);
        }

        if (!schemaExists(schemaName)) {
            throw new SchemaNotFoundException(schemaName);
        }

        jdbcTemplate.execute("DROP DATABASE " + schemaName);
        return !schemaExists(schemaName);
    }
}
