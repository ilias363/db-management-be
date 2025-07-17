package ma.ilias.dbmanagementbe.metadata.dto.column.standard;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.validation.NotNullOrUnique;
import ma.ilias.dbmanagementbe.validation.groups.StandaloneColumnCreation;

@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@NotNullOrUnique(groups = StandaloneColumnCreation.class)
public class NewStandardColumnDto extends BaseNewColumnDto {
    private Boolean isNullable;
    private Boolean isUnique;
    private String columnDefault;
    private Boolean autoIncrement;
}
