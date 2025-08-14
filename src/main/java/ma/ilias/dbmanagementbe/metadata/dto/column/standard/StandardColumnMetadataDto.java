package ma.ilias.dbmanagementbe.metadata.dto.column.standard;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseTableColumnMetadataDto;

@Data
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
public class StandardColumnMetadataDto extends BaseTableColumnMetadataDto {
    @Override
    public ColumnType getColumnType() {
        return ColumnType.STANDARD;
    }
}
