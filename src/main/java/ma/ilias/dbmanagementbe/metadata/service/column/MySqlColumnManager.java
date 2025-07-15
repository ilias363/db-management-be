package ma.ilias.dbmanagementbe.metadata.service.column;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.ColumnNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.ForeignKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.PrimaryKeyColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.StandardColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaService;
import ma.ilias.dbmanagementbe.metadata.service.table.TableService;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@AllArgsConstructor
@Transactional
public class MySqlColumnManager implements ColumnService {

    private final JdbcTemplate jdbcTemplate;
    private final SchemaService schemaService;
    private final TableService tableService;

    @Override
    public Boolean columnExists(String schemaName, String tableName, String columnName) {
        if (!tableService.tableExists(schemaName, tableName)) {
            throw new TableNotFoundException(schemaName.toLowerCase(), tableName.toLowerCase());
        }

        String sql = """
                SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS
                WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND COLUMN_NAME = ?
                """;
        Integer count = jdbcTemplate.queryForObject(sql, Integer.class, schemaName, tableName, columnName);
        return count != null && count > 0;
    }

    @Override
    public BaseColumnMetadataDto getColumn(String schemaName, String tableName, String columnName) {
        if (!columnExists(schemaName, tableName, columnName)) {
            throw new ColumnNotFoundException(schemaName, tableName, columnName);
        }

        String sql = """
                SELECT  c.COLUMN_NAME,
                        c.ORDINAL_POSITION,
                        c.DATA_TYPE,
                        c.CHARACTER_MAXIMUM_LENGTH,
                        c.NUMERIC_PRECISION,
                        c.NUMERIC_SCALE,
                        c.IS_NULLABLE,
                        c.COLUMN_DEFAULT,
                        c.COLUMN_KEY,
                        c.EXTRA
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
                (rs, rowNum) -> rs.getString("COLUMN_NAME")
        );

        // Get foreign key information
        String foreignKeySql = """
                SELECT kcu.REFERENCED_TABLE_SCHEMA,
                       kcu.REFERENCED_TABLE_NAME,
                       kcu.REFERENCED_COLUMN_NAME,
                       rc.UPDATE_RULE,
                       rc.DELETE_RULE
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
                        rs.getString("UPDATE_RULE")
                )
        );

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

        TableMetadataDto table = jdbcTemplate.queryForObject(
                tableSql,
                (rs, rowNum) -> TableMetadataDto.builder()
                        .tableName(rs.getString("TABLE_NAME"))
                        .columnCount(rs.getInt("COLUMN_COUNT"))
                        .rowCount(rs.getLong("TABLE_ROWS"))
                        .sizeInBytes(rs.getLong("SIZE_IN_BYTES"))
                        .schema(
                                SchemaMetadataDto.builder()
                                        .schemaName(schemaName.toLowerCase())
                                        .isSystemSchema(schemaService.isSystemSchemaByName(schemaName))
                                        .creationDate(null)
                                        .build()
                        )
                        .build(),
                schemaName,
                tableName
        );

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
                    Boolean autoIncrement = rs.getString("EXTRA") != null && rs.getString("EXTRA").toLowerCase().contains("auto_increment");
                    boolean isPrimaryKey = "PRI".equalsIgnoreCase(rs.getString("COLUMN_KEY"));
                    Boolean isNullable = !isPrimaryKey && "YES".equalsIgnoreCase(rs.getString("IS_NULLABLE"));
                    Boolean isUnique = isPrimaryKey || uniqueColumns.contains(colName);

                    if (isPrimaryKey) {
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
                schemaName,
                tableName,
                columnName
        );
    }

    // Helper record for foreign key information
    private record ForeignKeyInfo(String referencedSchemaName, String referencedTableName,
                                  String referencedColumnName, String onUpdateAction, String onDeleteAction) {
    }
}
