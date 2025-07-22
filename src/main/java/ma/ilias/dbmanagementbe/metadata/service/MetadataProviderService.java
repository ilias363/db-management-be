package ma.ilias.dbmanagementbe.metadata.service;

import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;

import java.util.List;

public interface MetadataProviderService {
    Boolean isSystemSchemaByName(String schemaName);

    Boolean isColumnPrimaryKey(String schemaName, String tableName, String columnName);

    Boolean schemaExists(String schemaName);

    Boolean tableExists(String schemaName, String tableName);

    Boolean columnExists(String schemaName, String tableName, String columnName);

    Boolean indexExists(String schemaName, String tableName, String indexName);

    SchemaMetadataDto getSchemaByName(String schemaName, boolean includeTables, boolean checkSchemaExists);

    TableMetadataDto getTable(String schemaName, String tableName, boolean includeSchema,
                              boolean includeColumns, boolean includeIndexes, boolean checkTableExists);

    BaseColumnMetadataDto getColumn(String schemaName, String tableName, String columnName,
                                    boolean includeTable, boolean checkColumnExists);

    IndexMetadataDto getIndex(String schemaName, String tableName, String indexName,
                              boolean includeTable, boolean checkIndexExists);

    List<SchemaMetadataDto> getAllSchemas(Boolean includeSystemSchemas);

    List<TableMetadataDto> getTablesBySchema(String schemaName, boolean includeSchema, boolean includeColumns,
                                             boolean includeIndexes, boolean checkSchemaExists);

    List<BaseColumnMetadataDto> getColumnsByTable(String schemaName, String tableName,
                                                  boolean includeTable, boolean checkTableExists);

    List<IndexMetadataDto> getIndexesByTable(String schemaName, String tableName,
                                             boolean includeTable, boolean checkTableExists);
}
