package ma.ilias.dbmanagementbe.metadata.dto.column;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class UpdateColumnNameDto {
    @NotBlank(message = "Schema name cannot be blank")
    private String schemaName;

    @NotBlank(message = "Table name cannot be blank")
    private String tableName;

    @NotBlank(message = "Updated column name cannot be blank")
    private String updatedColumnName;
}