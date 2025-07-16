package ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.metadata.dto.IReferencedColumnReference;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.validation.ExistingReferencedColumn;
import ma.ilias.dbmanagementbe.validation.MatchingForeignKeyType;

@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@ExistingReferencedColumn
@MatchingForeignKeyType
public class NewForeignKeyColumnDto extends BaseNewColumnDto implements IReferencedColumnReference {
    @NotBlank(message = "Referenced schema name cannot be blank")
    private String referencedSchemaName;

    @NotBlank(message = "Referenced table name cannot be blank")
    private String referencedTableName;

    @NotBlank(message = "Referenced column name cannot be blank")
    private String referencedColumnName;

    @Pattern(
            regexp = "^(?i)(CASCADE|NO ACTION|RESTRICT|SET NULL|SET DEFAULT)$",
            message = "Delete action is not valid"
    )
    private String onDeleteAction;

    @Pattern(
            regexp = "^(?i)(CASCADE|NO ACTION|RESTRICT|SET NULL|SET DEFAULT)$",
            message = "Update action is not valid"
    )
    private String onUpdateAction;
}
