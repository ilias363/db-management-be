package ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.validation.annotations.ValidForeignKeyDefault;

@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@ValidForeignKeyDefault
public class NewForeignKeyColumnDto extends BaseNewForeignKeyColumnDto {
    private String columnDefault;
    private Boolean isNullable;

    @Override
    public ColumnType getColumnType() {
        return ColumnType.FOREIGN_KEY;
    }
}
