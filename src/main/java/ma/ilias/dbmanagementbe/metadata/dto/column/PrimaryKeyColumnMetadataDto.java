package ma.ilias.dbmanagementbe.metadata.dto.column;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;

@Data
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
public class PrimaryKeyColumnMetadataDto extends BaseColumnMetadataDto {

    public PrimaryKeyColumnMetadataDto() {
        super();
    }

    public PrimaryKeyColumnMetadataDto(String columnName, Integer ordinalPosition, String dataType,
                                       Integer characterMaxLength, Integer numericPrecision, Integer numericScale,
                                       String columnDefault, Boolean autoIncrement, TableMetadataDto table) {
        super(columnName, ordinalPosition, dataType, characterMaxLength, numericPrecision, numericScale,
                false, true, columnDefault, autoIncrement, table);
    }

    @Override
    public ColumnType getColumnType() {
        return ColumnType.PRIMARY_KEY;
    }
}
