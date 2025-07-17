package ma.ilias.dbmanagementbe.metadata.dto.column.primarykey;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;

@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class NewPrimaryKeyColumnDto extends BaseNewColumnDto {
    private Boolean autoIncrement;
}
