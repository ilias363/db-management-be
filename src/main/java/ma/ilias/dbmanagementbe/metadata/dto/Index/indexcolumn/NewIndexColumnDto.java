package ma.ilias.dbmanagementbe.metadata.dto.index.indexcolumn;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.Data;

@Data
public class NewIndexColumnDto {
    @NotBlank(message = "Column name is required")
    private String columnName;

    @Pattern(regexp = "^(?i)(ASC|DESC)$", message = "Sort order is not valid (ASC, DESC)")
    private String sortOrder;
}
