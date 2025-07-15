package ma.ilias.dbmanagementbe.metadata.dto.column;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;

@Data
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
public class StandardColumnMetadataDto extends BaseColumnMetadataDto {

    public StandardColumnMetadataDto() {
        super();
    }

    public StandardColumnMetadataDto(String columnName, Integer ordinalPosition, String dataType,
                                     Long characterMaxLength, Integer numericPrecision, Integer numericScale,
                                     Boolean isNullable, Boolean isUnique, String columnDefault, Boolean autoIncrement,
                                     TableMetadataDto table) {
        super(columnName, ordinalPosition, dataType, characterMaxLength, numericPrecision, numericScale,
                isNullable, isUnique, columnDefault, autoIncrement, table);
    }

    @Override
    public ColumnType getColumnType() {
        return ColumnType.STANDARD;
    }
}
