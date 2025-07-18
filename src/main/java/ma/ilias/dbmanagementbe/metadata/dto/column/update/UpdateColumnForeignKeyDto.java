package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnReference;
import ma.ilias.dbmanagementbe.metadata.dto.common.IReferencedColumnReference;
import ma.ilias.dbmanagementbe.validation.ExistingColumn;
import ma.ilias.dbmanagementbe.validation.ExistingReferencedColumn;
import ma.ilias.dbmanagementbe.validation.ValidForeignKeyChange;

@Data
@ExistingColumn
@ExistingReferencedColumn
@ValidForeignKeyChange
public class UpdateColumnForeignKeyDto implements IColumnReference, IReferencedColumnReference {
    @NotBlank(message = "Schema name is required")
    private String schemaName;

    @NotBlank(message = "Table name is required")
    private String tableName;

    @NotBlank(message = "Column name is required")
    private String columnName;

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
