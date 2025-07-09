package ma.ilias.dbmanagementbe.metadata.dto.foreignkey;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class NewForeignKeyDto {
    @NotBlank(message = "Constraint name cannot be blank")
    private String constraintName;

    @NotBlank(message = "Table name cannot be blank")
    private String tableName;

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
