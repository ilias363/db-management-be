package ma.ilias.dbmanagementbe.metadata.dto.column.primarykey;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseTableColumnMetadataDto;

@Data
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
public class PrimaryKeyColumnMetadataDto extends BaseTableColumnMetadataDto {
    @Override
    public ColumnType getColumnType() {
        return ColumnType.PRIMARY_KEY;
    }
}
