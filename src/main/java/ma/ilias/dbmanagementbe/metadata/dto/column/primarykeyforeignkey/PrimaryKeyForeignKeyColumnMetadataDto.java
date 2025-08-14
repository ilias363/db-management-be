package ma.ilias.dbmanagementbe.metadata.dto.column.primarykeyforeignkey;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseTableColumnMetadataDto;

@Data
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
public class PrimaryKeyForeignKeyColumnMetadataDto extends BaseTableColumnMetadataDto {

    private String referencedSchemaName;
    private String referencedTableName;
    private String referencedColumnName;
    private String onDeleteAction;
    private String onUpdateAction;

    @Override
    public ColumnType getColumnType() {
        return ColumnType.PRIMARY_KEY_FOREIGN_KEY;
    }
}
