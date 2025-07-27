package ma.ilias.dbmanagementbe.metadata.service;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.enums.IndexType;
import ma.ilias.dbmanagementbe.exception.*;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.ForeignKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.PrimaryKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykeyforeignkey.PrimaryKeyForeignKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.StandardColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.indexcolumn.IndexColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.view.ViewColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.view.ViewMetadataDto;
import ma.ilias.dbmanagementbe.util.SqlSecurityUtils;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Service
@AllArgsConstructor
@Transactional
public class MetadataProviderManager implements MetadataProviderService {

    private final JdbcTemplate jdbcTemplate;

    public Boolean isSystemSchemaByName(String schemaName) {
        return List.of("mysql", "sys", "information_schema", "performance_schema")
                .contains(schemaName.trim().toLowerCase());
    }

    public Boolean isColumnPrimaryKey(String schemaName, String tableName, String columnName) {
        String pkCheckSql = """
                SELECT COUNT(*) FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND COLUMN_NAME = ?
                  AND CONSTRAINT_NAME = 'PRIMARY'
                """;

        Integer pkCount = jdbcTemplate.queryForObject(pkCheckSql, Integer.class, schemaName, tableName, columnName);
        return pkCount != null && pkCount > 0;
    }

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

    public Boolean tableExists(String schemaName, String tableName) {
        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(schemaName);
        String validatedTableName = SqlSecurityUtils.validateTableName(tableName);

        if (!schemaExists(validatedSchemaName)) {
            throw new SchemaNotFoundException(validatedSchemaName.toLowerCase());
        }

        String tableSql = """
                SELECT TABLE_NAME
                FROM INFORMATION_SCHEMA.TABLES
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ?
                """;

        List<String> tables = jdbcTemplate.query(
                tableSql,
                ps -> {
                    ps.setString(1, validatedSchemaName);
                    ps.setString(2, validatedTableName);
                },
                (rs, rowNum) -> rs.getString("TABLE_NAME"));

        return !tables.isEmpty();
    }

    public Boolean columnExists(String schemaName, String tableName, String columnName) {
        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(schemaName);
        String validatedTableName = SqlSecurityUtils.validateTableName(tableName);
        String validatedColumnName = SqlSecurityUtils.validateColumnName(columnName);

        if (!tableExists(validatedSchemaName, validatedTableName)) {
            throw new TableNotFoundException(validatedSchemaName.toLowerCase(), validatedTableName.toLowerCase());
        }

        String sql = """
                SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND COLUMN_NAME = ?
                """;
        Integer count = jdbcTemplate.queryForObject(sql, Integer.class, validatedSchemaName, validatedTableName, validatedColumnName);
        return count != null && count > 0;
    }

    public Boolean indexExists(String schemaName, String tableName, String indexName) {
        if (!tableExists(schemaName, tableName)) {
            throw new TableNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase());
        }

        String sql = """
                SELECT COUNT(*) FROM INFORMATION_SCHEMA.STATISTICS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND INDEX_NAME = ?
                """;

        Integer count = jdbcTemplate.queryForObject(sql, Integer.class,
                SqlSecurityUtils.validateSchemaName(schemaName),
                SqlSecurityUtils.validateTableName(tableName),
                SqlSecurityUtils.validateIndexName(indexName));
        return count != null && count > 0;
    }

    public Boolean viewExists(String schemaName, String viewName) {
        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(schemaName);
        String validatedViewName = SqlSecurityUtils.validateTableName(viewName);

        if (!schemaExists(validatedSchemaName)) {
            throw new SchemaNotFoundException(validatedSchemaName.toLowerCase());
        }

        String sql = """
                SELECT COUNT(*) FROM INFORMATION_SCHEMA.VIEWS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ?
                """;

        Integer count = jdbcTemplate.queryForObject(sql, Integer.class, validatedSchemaName, validatedViewName);
        return count != null && count > 0;
    }

    public SchemaMetadataDto getSchemaByName(String schemaName, boolean includeTables, boolean checkSchemaExists) {
        if (checkSchemaExists && !schemaExists(schemaName)) {
            throw new SchemaNotFoundException(schemaName);
        }

        return SchemaMetadataDto.builder()
                .schemaName(schemaName.toLowerCase())
                .isSystemSchema(isSystemSchemaByName(schemaName))
                .creationDate(null)
                .tables(includeTables ?
                        getTablesBySchema(schemaName, false, false, false, false)
                        : null)
                .build();
    }

    public TableMetadataDto getTable(String schemaName, String tableName, boolean includeSchema,
                                     boolean includeColumns, boolean includeIndexes, boolean checkTableExists) {
        if (checkTableExists && !tableExists(schemaName, tableName)) {
            throw new TableNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase());
        }

        String tableSql = """
                SELECT t.TABLE_NAME,
                       COUNT(c.COLUMN_NAME) AS COLUMN_COUNT,
                       t.TABLE_ROWS,
                       (t.DATA_LENGTH + t.INDEX_LENGTH) AS SIZE_IN_BYTES
                FROM INFORMATION_SCHEMA.TABLES t
                LEFT JOIN INFORMATION_SCHEMA.COLUMNS c
                    ON t.TABLE_SCHEMA = c.TABLE_SCHEMA AND t.TABLE_NAME = c.TABLE_NAME
                WHERE t.TABLE_SCHEMA = ? AND t.TABLE_NAME = ?
                GROUP BY t.TABLE_NAME, t.TABLE_ROWS, t.DATA_LENGTH, t.INDEX_LENGTH
                """;

        return jdbcTemplate.queryForObject(
                tableSql,
                (rs, rowNum) -> TableMetadataDto.builder()
                        .tableName(rs.getString("TABLE_NAME"))
                        .columnCount(rs.getInt("COLUMN_COUNT"))
                        .rowCount(rs.getLong("TABLE_ROWS"))
                        .sizeInBytes(rs.getLong("SIZE_IN_BYTES"))
                        .schema(includeSchema ? getSchemaByName(schemaName, false, false) : null)
                        .columns(includeColumns ? getColumnsByTable(schemaName, tableName, false, false) : null)
                        .indexes(includeIndexes ? getIndexesByTable(schemaName, tableName, false, false) : null)
                        .build(),
                schemaName,
                tableName);
    }

    public BaseColumnMetadataDto getColumn(String schemaName, String tableName, String columnName,
                                           boolean includeTable, boolean checkColumnExists) {
        if (checkColumnExists && !columnExists(schemaName, tableName, columnName)) {
            throw new ColumnNotFoundException(schemaName, tableName, columnName);
        }

        String sql = """
                SELECT  c.COLUMN_NAME, c.ORDINAL_POSITION, c.DATA_TYPE,
                        c.CHARACTER_MAXIMUM_LENGTH, c.NUMERIC_PRECISION, c.NUMERIC_SCALE,
                        c.IS_NULLABLE, c.COLUMN_DEFAULT, c.COLUMN_KEY, c.EXTRA
                FROM INFORMATION_SCHEMA.COLUMNS c
                WHERE c.TABLE_SCHEMA = ? AND c.TABLE_NAME = ? AND c.COLUMN_NAME = ?
                """;

        String uniqueColsSql = """
                SELECT c.COLUMN_NAME
                FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE c
                JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS t
                  ON c.CONSTRAINT_NAME = t.CONSTRAINT_NAME
                WHERE c.TABLE_SCHEMA = ? AND c.TABLE_NAME = ? AND t.CONSTRAINT_TYPE = 'UNIQUE'
                  AND c.COLUMN_NAME = ?
                """;

        List<String> uniqueColumns = jdbcTemplate.query(
                uniqueColsSql,
                ps -> {
                    ps.setString(1, schemaName);
                    ps.setString(2, tableName);
                    ps.setString(3, columnName);
                },
                (rs, rowNum) -> rs.getString("COLUMN_NAME"));

        // Get foreign key information
        String foreignKeySql = """
                SELECT kcu.REFERENCED_TABLE_SCHEMA, kcu.REFERENCED_TABLE_NAME, kcu.REFERENCED_COLUMN_NAME,
                       rc.UPDATE_RULE, rc.DELETE_RULE
                FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE kcu
                JOIN INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS rc
                    ON kcu.CONSTRAINT_NAME = rc.CONSTRAINT_NAME
                    AND kcu.CONSTRAINT_SCHEMA = rc.CONSTRAINT_SCHEMA
                WHERE kcu.TABLE_SCHEMA = ? AND kcu.TABLE_NAME = ? AND kcu.COLUMN_NAME = ?
                """;

        List<ForeignKeyInfo> foreignKeyInfos = jdbcTemplate.query(
                foreignKeySql,
                ps -> {
                    ps.setString(1, schemaName);
                    ps.setString(2, tableName);
                    ps.setString(3, columnName);
                },
                (rs, rowNum) -> new ForeignKeyInfo(
                        rs.getString("REFERENCED_TABLE_SCHEMA"),
                        rs.getString("REFERENCED_TABLE_NAME"),
                        rs.getString("REFERENCED_COLUMN_NAME"),
                        rs.getString("DELETE_RULE"),
                        rs.getString("UPDATE_RULE")));

        return jdbcTemplate.queryForObject(
                sql,
                (rs, rowNum) -> {
                    String colName = rs.getString("COLUMN_NAME");
                    Integer ordinalPosition = rs.getObject("ORDINAL_POSITION", Integer.class);
                    String dataType = rs.getString("DATA_TYPE");
                    Long characterMaxLength = rs.getObject("CHARACTER_MAXIMUM_LENGTH", Long.class);
                    Integer numericPrecision = rs.getObject("NUMERIC_PRECISION", Integer.class);
                    Integer numericScale = rs.getObject("NUMERIC_SCALE", Integer.class);
                    String columnDefault = rs.getString("COLUMN_DEFAULT");
                    Boolean autoIncrement = rs.getString("EXTRA") != null
                            && rs.getString("EXTRA").toLowerCase().contains("auto_increment");
                    boolean isPrimaryKey = "PRI".equalsIgnoreCase(rs.getString("COLUMN_KEY"));
                    Boolean isNullable = !isPrimaryKey && "YES".equalsIgnoreCase(rs.getString("IS_NULLABLE"));
                    Boolean isUnique = isPrimaryKey || uniqueColumns.contains(colName);

                    TableMetadataDto table = includeTable ? getTable(schemaName, tableName, true, false, false, false) : null;

                    if (isPrimaryKey && !foreignKeyInfos.isEmpty()) {
                        return PrimaryKeyForeignKeyColumnMetadataDto.builder()
                                .columnName(colName)
                                .ordinalPosition(ordinalPosition)
                                .dataType(dataType)
                                .characterMaxLength(characterMaxLength)
                                .numericPrecision(numericPrecision)
                                .numericScale(numericScale)
                                .isNullable(false)
                                .isUnique(true)
                                .columnDefault(columnDefault)
                                .autoIncrement(autoIncrement)
                                .table(table)
                                .referencedSchemaName(foreignKeyInfos.get(0).referencedSchemaName())
                                .referencedTableName(foreignKeyInfos.get(0).referencedTableName())
                                .referencedColumnName(foreignKeyInfos.get(0).referencedColumnName())
                                .onDeleteAction(foreignKeyInfos.get(0).onDeleteAction())
                                .onUpdateAction(foreignKeyInfos.get(0).onUpdateAction())
                                .build();
                    } else if (isPrimaryKey) {
                        return PrimaryKeyColumnMetadataDto.builder()
                                .columnName(colName)
                                .ordinalPosition(ordinalPosition)
                                .dataType(dataType)
                                .characterMaxLength(characterMaxLength)
                                .numericPrecision(numericPrecision)
                                .numericScale(numericScale)
                                .isNullable(false)
                                .isUnique(true)
                                .columnDefault(columnDefault)
                                .autoIncrement(autoIncrement)
                                .table(table)
                                .build();
                    } else if (!foreignKeyInfos.isEmpty()) {
                        ForeignKeyInfo fkInfo = foreignKeyInfos.get(0);
                        return ForeignKeyColumnMetadataDto.builder()
                                .columnName(colName)
                                .ordinalPosition(ordinalPosition)
                                .dataType(dataType)
                                .characterMaxLength(characterMaxLength)
                                .numericPrecision(numericPrecision)
                                .numericScale(numericScale)
                                .isNullable(isNullable)
                                .isUnique(isUnique)
                                .columnDefault(columnDefault)
                                .autoIncrement(autoIncrement)
                                .table(table)
                                .referencedSchemaName(fkInfo.referencedSchemaName())
                                .referencedTableName(fkInfo.referencedTableName())
                                .referencedColumnName(fkInfo.referencedColumnName())
                                .onDeleteAction(fkInfo.onDeleteAction())
                                .onUpdateAction(fkInfo.onUpdateAction())
                                .build();
                    } else {
                        return StandardColumnMetadataDto.builder()
                                .columnName(colName)
                                .ordinalPosition(ordinalPosition)
                                .dataType(dataType)
                                .characterMaxLength(characterMaxLength)
                                .numericPrecision(numericPrecision)
                                .numericScale(numericScale)
                                .isNullable(isNullable)
                                .isUnique(isUnique)
                                .columnDefault(columnDefault)
                                .autoIncrement(autoIncrement)
                                .table(table)
                                .build();
                    }
                },
                schemaName, tableName, columnName);
    }

    public IndexMetadataDto getIndex(String schemaName, String tableName, String indexName,
                                     boolean includeTable, boolean checkIndexExists) {
        if (checkIndexExists && !indexExists(schemaName, tableName, indexName)) {
            throw new IndexNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase(), indexName.toLowerCase());
        }

        String indexSql = """
                SELECT INDEX_NAME, NON_UNIQUE, INDEX_TYPE
                FROM INFORMATION_SCHEMA.STATISTICS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND INDEX_NAME = ?
                GROUP BY INDEX_NAME, NON_UNIQUE, INDEX_TYPE
                """;

        String indexColumnsSql = """
                SELECT COLUMN_NAME, SEQ_IN_INDEX, COLLATION
                FROM INFORMATION_SCHEMA.STATISTICS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND INDEX_NAME = ?
                ORDER BY SEQ_IN_INDEX
                """;

        List<IndexColumnMetadataDto> indexColumns = jdbcTemplate.query(
                indexColumnsSql,
                (rs, rowNum) -> {
                    String sortOrder = rs.getString("COLLATION");
                    if (sortOrder != null) {
                        if ("A".equals(sortOrder)) {
                            sortOrder = "ASC";
                        } else if ("D".equals(sortOrder)) {
                            sortOrder = "DESC";
                        }
                    }

                    return IndexColumnMetadataDto.builder()
                            .columnName(rs.getString("COLUMN_NAME"))
                            .ordinalPosition(rs.getInt("SEQ_IN_INDEX"))
                            .sortOrder(sortOrder)
                            .build();
                },
                schemaName, tableName, indexName);

        return jdbcTemplate.queryForObject(
                indexSql,
                (rs, rowNum) -> {
                    String indexTypeStr = rs.getString("INDEX_TYPE");
                    IndexType indexType = null;
                    if (indexTypeStr != null) {
                        try {
                            indexType = IndexType.valueOf(indexTypeStr.toUpperCase());
                        } catch (IllegalArgumentException ignored) {
                        }
                    }

                    return IndexMetadataDto.builder()
                            .indexName(rs.getString("INDEX_NAME"))
                            .isUnique(!rs.getBoolean("NON_UNIQUE"))
                            .indexType(indexType)
                            .table(includeTable ?
                                    getTable(schemaName, tableName, true, false, false, false)
                                    : null)
                            .indexColumns(indexColumns)
                            .build();
                },
                schemaName, tableName, indexName);
    }

    public ViewMetadataDto getView(String schemaName, String viewName, boolean includeSchema,
                                   boolean includeColumns, boolean checkViewExists) {
        if (checkViewExists && !viewExists(schemaName, viewName)) {
            throw new ViewNotFoundException(schemaName.toLowerCase(), viewName.toLowerCase());
        }

        String sql = """
                SELECT TABLE_NAME as VIEW_NAME,
                       VIEW_DEFINITION,
                       CHECK_OPTION,
                       IS_UPDATABLE,
                       CHARACTER_SET_CLIENT,
                       COLLATION_CONNECTION
                FROM INFORMATION_SCHEMA.VIEWS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ?
                """;

        return jdbcTemplate.queryForObject(sql, (rs, rowNum) -> ViewMetadataDto.builder()
                .viewName(rs.getString("VIEW_NAME"))
                .viewDefinition(rs.getString("VIEW_DEFINITION"))
                .checkOption(rs.getString("CHECK_OPTION"))
                .isUpdatable("YES".equalsIgnoreCase(rs.getString("IS_UPDATABLE")))
                .characterSet(rs.getString("CHARACTER_SET_CLIENT"))
                .collation(rs.getString("COLLATION_CONNECTION"))
                .schema(includeSchema ? getSchemaByName(schemaName, false, false) : null)
                .columns(includeColumns ? getColumnsByView(schemaName, viewName, false, false) : null)
                .build(), schemaName, viewName);
    }

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

    public List<TableMetadataDto> getTablesBySchema(String schemaName, boolean includeSchema, boolean includeColumns,
                                                    boolean includeIndexes, boolean checkSchemaExists) {
        if (checkSchemaExists && !schemaExists(schemaName)) {
            throw new SchemaNotFoundException(schemaName.toLowerCase());
        }

        String tableSql = """
                SELECT TABLE_NAME
                FROM INFORMATION_SCHEMA.TABLES
                WHERE TABLE_SCHEMA = ?
                """;

        var tables = jdbcTemplate.query(
                tableSql,
                (rs, rowNum) -> getTable(
                        schemaName, rs.getString("TABLE_NAME"),
                        false, includeColumns, includeIndexes, false),
                schemaName);

        if (includeSchema) {
            var schema = getSchemaByName(schemaName, false, false);
            tables.forEach(table -> table.setSchema(schema));
        }

        return tables;
    }

    public List<ViewMetadataDto> getViewsBySchema(String schemaName, boolean includeSchema, boolean includeColumns,
                                                  boolean checkSchemaExists) {
        if (checkSchemaExists && !schemaExists(schemaName)) {
            throw new SchemaNotFoundException(schemaName.toLowerCase());
        }

        String sql = """
                SELECT TABLE_NAME as VIEW_NAME
                FROM INFORMATION_SCHEMA.VIEWS
                WHERE TABLE_SCHEMA = ?
                ORDER BY TABLE_NAME
                """;

        var views = jdbcTemplate.query(
                sql,
                (rs, rowNum) -> getView(schemaName, rs.getString("VIEW_NAME"),
                        false, includeColumns, false),
                schemaName);

        if (includeSchema) {
            var schema = getSchemaByName(schemaName, false, false);
            views.forEach(view -> view.setSchema(schema));
        }

        return views;
    }

    public List<BaseColumnMetadataDto> getColumnsByTable(String schemaName, String tableName,
                                                         boolean includeTable, boolean checkTableExists) {
        if (checkTableExists && !tableExists(schemaName, tableName)) {
            throw new TableNotFoundException(schemaName, tableName);
        }

        String columnsSql = """
                SELECT  c.COLUMN_NAME
                FROM INFORMATION_SCHEMA.COLUMNS c
                WHERE c.TABLE_SCHEMA = ? AND c.TABLE_NAME = ?
                ORDER BY c.ORDINAL_POSITION
                """;

        List<BaseColumnMetadataDto> columns = jdbcTemplate.query(
                columnsSql,
                (rs, rowNum) -> getColumn(schemaName, tableName, rs.getString("COLUMN_NAME"), false, false),
                schemaName, tableName);

        if (includeTable) {
            TableMetadataDto table = getTable(schemaName, tableName, true, false, false, false);
            columns.forEach(col -> col.setTable(table));
        }

        return columns;
    }

    public List<ViewColumnMetadataDto> getColumnsByView(String schemaName, String viewName,
                                                        boolean includeView, boolean checkViewExists) {
        if (checkViewExists && !viewExists(schemaName, viewName)) {
            throw new ViewNotFoundException(schemaName, viewName);
        }

        var columns = getColumnsByTable(schemaName, viewName, false, false)
                .stream().map(this::mapBaseColumnToViewColumn).toList();

        if (includeView) {
            var view = getView(schemaName, viewName, true, false, false);
            columns.forEach(col -> col.setView(view));
        }
        return columns;
    }

    private ViewColumnMetadataDto mapBaseColumnToViewColumn(BaseColumnMetadataDto column) {
        return ViewColumnMetadataDto.builder()
                .columnName(column.getColumnName())
                .ordinalPosition(column.getOrdinalPosition())
                .dataType(column.getDataType())
                .characterMaxLength(column.getCharacterMaxLength())
                .numericPrecision(column.getNumericPrecision())
                .numericScale(column.getNumericScale())
                .isNullable(column.getIsNullable())
                .isUnique(column.getIsUnique())
                .columnDefault(column.getColumnDefault())
                .autoIncrement(column.getAutoIncrement())
                .build();
    }

    public List<IndexMetadataDto> getIndexesByTable(String schemaName, String tableName,
                                                    boolean includeTable, boolean checkTableExists) {
        if (checkTableExists && !tableExists(schemaName, tableName)) {
            throw new TableNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase());
        }

        String indexesSql = """
                SELECT INDEX_NAME
                FROM INFORMATION_SCHEMA.STATISTICS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ?
                ORDER BY INDEX_NAME
                """;

        var indexes = jdbcTemplate.query(
                indexesSql,
                (rs, rowNum) -> getIndex(schemaName, tableName, rs.getString("INDEX_NAME"), false, false),
                schemaName, tableName);

        if (includeTable) {
            TableMetadataDto table = getTable(schemaName, tableName, true, false, false, false);
            indexes.forEach(index -> index.setTable(table));
        }
        return indexes;
    }

    private record ForeignKeyInfo(String referencedSchemaName, String referencedTableName,
                                  String referencedColumnName, String onUpdateAction, String onDeleteAction) {
    }
}
