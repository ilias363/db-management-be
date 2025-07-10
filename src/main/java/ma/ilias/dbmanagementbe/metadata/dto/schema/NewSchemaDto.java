package ma.ilias.dbmanagementbe.metadata.dto.schema;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import ma.ilias.dbmanagementbe.validation.UniqueSchemaName;

@Data
public class NewSchemaDto {
    @NotBlank(message = "Schema name cannot be blank")
    @UniqueSchemaName
    @Pattern(
            regexp = "^[a-zA-Z][a-zA-Z0-9_]*$",
            message = "Schema name must start with a letter and contain only alphanumeric characters and underscores"
    )
    private String schemaName;
}
