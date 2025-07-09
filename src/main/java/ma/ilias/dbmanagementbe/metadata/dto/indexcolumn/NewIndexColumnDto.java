package ma.ilias.dbmanagementbe.metadata.dto.indexcolumn;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class NewIndexColumnDto {
    @NotBlank(message = "Column name is required")
    private String columnName;

    @Pattern(
            regexp = "^(?i)(ASC|DESC)$",
            message = "Sort order is not valid (ASC, DESC, null)"
    )
    private String sortOrder;
}
