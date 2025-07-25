package ma.ilias.dbmanagementbe.metadata.service.index;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.IndexNotFoundException;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.metadata.dto.index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.NewIndexDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.service.DatabaseAuthorizationService;
import ma.ilias.dbmanagementbe.util.SqlSecurityUtils;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
@Transactional
public class MySqlIndexManager implements IndexService {

    private final JdbcTemplate jdbcTemplate;
    private final MetadataProviderService metadataProviderService;
    private final DatabaseAuthorizationService databaseAuthorizationService;

    @Override
    public Boolean indexExists(String schemaName, String tableName, String indexName) {
        return metadataProviderService.indexExists(schemaName, tableName, indexName);
    }

    @Override
    public IndexMetadataDto getIndex(
            String schemaName, String tableName, String indexName,
            boolean includeTable, boolean checkIndexExists) {
        return getIndex(schemaName, tableName, indexName, includeTable, checkIndexExists, false);
    }

    @Override
    public IndexMetadataDto getIndex(
            String schemaName, String tableName, String indexName,
            boolean includeTable, boolean checkIndexExists, boolean checkAuthorization) {
        if (checkAuthorization) {
            databaseAuthorizationService.checkReadPermission(schemaName, tableName);
        }
        return metadataProviderService.getIndex(schemaName, tableName, indexName, includeTable, checkIndexExists);
    }

    @Override
    public List<IndexMetadataDto> getIndexesByTable(
            String schemaName, String tableName,
            boolean includeTable, boolean checkTableExists) {
        databaseAuthorizationService.checkReadPermission(schemaName, tableName);
        return metadataProviderService.getIndexesByTable(schemaName, tableName, includeTable, checkTableExists);
    }

    @Override
    public IndexMetadataDto createIndex(NewIndexDto newIndexDto) {
        databaseAuthorizationService.checkCreatePermission(newIndexDto.getSchemaName(), newIndexDto.getTableName());

        StringBuilder createIndexSql = new StringBuilder("CREATE ");

        if (Boolean.TRUE.equals(newIndexDto.getIsUnique())) {
            createIndexSql.append("UNIQUE ");
        }

        createIndexSql.append("INDEX ").append(SqlSecurityUtils.validateIndexName(newIndexDto.getIndexName()));

        createIndexSql.append(" ON ")
                .append(SqlSecurityUtils.validateSchemaName(newIndexDto.getSchemaName()))
                .append(".")
                .append(SqlSecurityUtils.validateTableName(newIndexDto.getTableName()))
                .append(" (");

        String columnsPart = newIndexDto.getIndexColumns().stream()
                .map(col -> {
                    StringBuilder colSql = new StringBuilder(SqlSecurityUtils.validateColumnName(col.getColumnName()));
                    if (col.getSortOrder() != null && !col.getSortOrder().isBlank()) {
                        colSql.append(" ").append(col.getSortOrder().toUpperCase());
                    }
                    return colSql.toString();
                })
                .collect(Collectors.joining(", "));

        createIndexSql.append(columnsPart).append(")");

        createIndexSql.append(" USING ").append(newIndexDto.getIndexType().toUpperCase());

        jdbcTemplate.execute(createIndexSql.toString());

        return getIndex(newIndexDto.getSchemaName(), newIndexDto.getTableName(), newIndexDto.getIndexName(),
                true, false);
    }

    @Override
    public Boolean deleteIndex(String schemaName, String tableName, String indexName) {
        databaseAuthorizationService.checkDeletePermission(schemaName, tableName);

        if (!indexExists(schemaName, tableName, indexName)) {
            throw new IndexNotFoundException(schemaName, tableName, indexName);
        }

        if ("PRIMARY".equalsIgnoreCase(indexName)) {
            throw new UnauthorizedActionException(
                    "Cannot drop PRIMARY KEY index. Use the table's primary key management instead.");

        } else {
            String sql = String.format("DROP INDEX %s ON %s.%s",
                    SqlSecurityUtils.validateIndexName(indexName),
                    SqlSecurityUtils.validateSchemaName(schemaName),
                    SqlSecurityUtils.validateTableName(tableName));
            jdbcTemplate.execute(sql);
        }

        return !indexExists(schemaName, tableName, indexName);
    }
}
