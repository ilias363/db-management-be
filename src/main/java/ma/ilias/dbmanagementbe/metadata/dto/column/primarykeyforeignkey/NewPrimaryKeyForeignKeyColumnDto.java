package ma.ilias.dbmanagementbe.metadata.dto.column.primarykeyforeignkey;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewForeignKeyColumnDto;

@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class NewPrimaryKeyForeignKeyColumnDto extends BaseNewForeignKeyColumnDto {
    @Override
    public ColumnType getColumnType() {
        return ColumnType.PRIMARY_KEY_FOREIGN_KEY;
    }
}
