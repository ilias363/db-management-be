package ma.ilias.dbmanagementbe.metadata.dto.column.primarykeyforeignkey;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewForeignKeyColumnDto;

@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class NewPrimaryKeyForeignKeyColumnDto extends BaseNewForeignKeyColumnDto {
}
