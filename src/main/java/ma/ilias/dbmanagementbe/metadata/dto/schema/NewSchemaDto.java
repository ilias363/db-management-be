package ma.ilias.dbmanagementbe.metadata.dto.schema;

import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class NewSchemaDto {
    @NotBlank(message = "Schema name cannot be blank")
    private String schemaName;
}
