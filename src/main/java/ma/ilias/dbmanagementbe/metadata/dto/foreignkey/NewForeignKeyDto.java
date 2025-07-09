package ma.ilias.dbmanagementbe.metadata.dto.foreignkey;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class NewForeignKeyDto {
    @NotBlank(message = "Constraint name cannot be blank")
    private String constraintName;

    @NotBlank(message = "Schema name cannot be blank")
    private String schemaName;

    @NotBlank(message = "From table name cannot be blank")
    private String fromTableName;

    @NotBlank(message = "To table name cannot be blank")
    private String toTableName;

    @NotBlank(message = "From column name cannot be blank")
    private String fromColumnName;

    @NotBlank(message = "To column name cannot be blank")
    private String toColumnName;

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
