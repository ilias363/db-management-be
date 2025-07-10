package ma.ilias.dbmanagementbe.metadata.dto.column;

import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Data
public class UpdateColumnUniqueDto {
    @NotNull(message = "The isUnique field cannot be null.")
    private Boolean isUnique;
}
