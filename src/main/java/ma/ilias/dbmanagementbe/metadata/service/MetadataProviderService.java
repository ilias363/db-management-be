package ma.ilias.dbmanagementbe.metadata.service;

import ma.ilias.dbmanagementbe.metadata.dto.column.BaseTableColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.view.ViewColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.view.ViewMetadataDto;

import java.util.List;
import java.util.Map;

public interface MetadataProviderService {
    Boolean isSystemSchemaByName(String schemaName);

    Boolean isColumnPrimaryKey(String schemaName, String tableName, String columnName);

    Boolean schemaExists(String schemaName);

    Boolean tableExists(String schemaName, String tableName);

    Boolean columnExists(String schemaName, String tableName, String columnName);

    Boolean viewColumnExists(String schemaName, String viewName, String columnName);

    Boolean indexExists(String schemaName, String tableName, String indexName);

    Boolean viewExists(String schemaName, String viewName);

    Boolean tableOrViewExists(String schemaName, String objectName);

    SchemaMetadataDto getSchemaByName(String schemaName, boolean includeTables, boolean includeViews, boolean checkSchemaExists);

    SchemaMetadataDto getSchemaByName(String schemaName, List<String> tableNames, List<String> viewNames,
                                      boolean includeTables, boolean includeViews, boolean checkSchemaExists);

    TableMetadataDto getTable(String schemaName, String tableName, boolean includeSchema,
                              boolean includeColumns, boolean includeIndexes, boolean checkTableExists);

    BaseTableColumnMetadataDto getColumn(String schemaName, String tableName, String columnName,
                                         boolean includeTable, boolean checkColumnExists);

    IndexMetadataDto getIndex(String schemaName, String tableName, String indexName,
                              boolean includeTable, boolean checkIndexExists);

    ViewMetadataDto getView(String schemaName, String viewName, boolean includeSchema,
                            boolean includeColumns, boolean checkViewExists);

    List<SchemaMetadataDto> getAllSchemas(Map<String, Map<String, List<String>>> schemasInfo,
                                          boolean includeSystemSchemas, boolean includeTables, boolean includeViews);

    List<SchemaMetadataDto> getAllSchemas(boolean includeSystemSchemas, boolean includeTables, boolean includeViews);

    List<TableMetadataDto> getTablesBySchema(String schemaName, boolean includeSchema, boolean includeColumns,
                                             boolean includeIndexes, boolean checkSchemaExists);

    List<TableMetadataDto> getTablesBySchema(String schemaName, List<String> tableNames, boolean includeSchema, boolean includeColumns,
                                             boolean includeIndexes, boolean checkSchemaExists);

    List<ViewMetadataDto> getViewsBySchema(String schemaName,
                                           boolean includeSchema, boolean includeColumns, boolean checkSchemaExists);

    List<ViewMetadataDto> getViewsBySchema(String schemaName, List<String> viewNames,
                                           boolean includeSchema, boolean includeColumns, boolean checkSchemaExists);

    List<BaseTableColumnMetadataDto> getColumnsByTable(String schemaName, String tableName,
                                                       boolean includeTable, boolean checkTableExists);

    List<ViewColumnMetadataDto> getColumnsByView(String schemaName, String tableName,
                                                 boolean includeView, boolean checkViewExists);

    List<IndexMetadataDto> getIndexesByTable(String schemaName, String tableName,
                                             boolean includeTable, boolean checkTableExists);
}
