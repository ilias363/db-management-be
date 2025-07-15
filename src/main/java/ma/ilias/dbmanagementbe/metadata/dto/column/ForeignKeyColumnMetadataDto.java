package ma.ilias.dbmanagementbe.metadata.dto.column;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;

@Data
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
public class ForeignKeyColumnMetadataDto extends BaseColumnMetadataDto {
    private String referencedSchemaName;
    private String referencedTableName;
    private String referencedColumnName;
    private String onDeleteAction;
    private String onUpdateAction;

    public ForeignKeyColumnMetadataDto() {
        super();
    }

    public ForeignKeyColumnMetadataDto(String columnName, Integer ordinalPosition, String dataType,
                                       Long characterMaxLength, Integer numericPrecision, Integer numericScale,
                                       Boolean isNullable, Boolean isUnique, String columnDefault, Boolean autoIncrement,
                                       TableMetadataDto table, String referencedSchemaName,
                                       String referencedTableName, String referencedColumnName,
                                       String onDeleteAction, String onUpdateAction) {
        super(columnName, ordinalPosition, dataType, characterMaxLength, numericPrecision, numericScale,
                isNullable, isUnique, columnDefault, autoIncrement, table);
        this.referencedSchemaName = referencedSchemaName;
        this.referencedTableName = referencedTableName;
        this.referencedColumnName = referencedColumnName;
        this.onDeleteAction = onDeleteAction;
        this.onUpdateAction = onUpdateAction;
    }

    @Override
    public ColumnType getColumnType() {
        return ColumnType.FOREIGN_KEY;
    }
}
