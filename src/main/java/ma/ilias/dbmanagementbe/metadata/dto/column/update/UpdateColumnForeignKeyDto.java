package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import lombok.EqualsAndHashCode;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseUpdateColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.common.IReferencedColumnReference;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingReferencedColumn;
import ma.ilias.dbmanagementbe.validation.annotations.ValidForeignKeyChange;

@Data
@EqualsAndHashCode(callSuper = true)
@ExistingReferencedColumn
@ValidForeignKeyChange
public class UpdateColumnForeignKeyDto extends BaseUpdateColumnDto implements IReferencedColumnReference {
    @NotNull(message = "Foreign key value is required")
    private Boolean isForeignKey;

    private String referencedSchemaName;

    private String referencedTableName;

    private String referencedColumnName;

    @Pattern(regexp = "^(?i)(CASCADE|NO ACTION|RESTRICT|SET NULL|SET DEFAULT)$",
            message = "On update action is not valid")
    private String onUpdateAction;

    @Pattern(regexp = "^(?i)(CASCADE|NO ACTION|RESTRICT|SET NULL|SET DEFAULT)$",
            message = "On delete action is not valid")
    private String onDeleteAction;
}
