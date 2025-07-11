package ma.ilias.dbmanagementbe.metadata.dto.table;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import ma.ilias.dbmanagementbe.validation.UniqueTableName;

@Data
@UniqueTableName
public class UpdateTableDto implements TableDtoBase {
    @NotBlank(message = "Schema name cannot be blank")
    // schema existence checked in @UniqueTableName
    private String schemaName;

    @NotBlank(message = "Old table name cannot be blank")
    private String oldTableName;

    @NotBlank(message = "Updated table name cannot be blank")
    private String updatedTableName;
}
