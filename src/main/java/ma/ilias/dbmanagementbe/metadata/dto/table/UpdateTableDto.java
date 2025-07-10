package ma.ilias.dbmanagementbe.metadata.dto.table;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class UpdateTableDto {
    @NotBlank(message = "Schema name cannot be blank")
    private String schemaName;

    @NotBlank(message = "Updated table name cannot be blank")
    private String updatedTableName;
}
