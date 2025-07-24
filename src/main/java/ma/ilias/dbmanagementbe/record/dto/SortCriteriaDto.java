package ma.ilias.dbmanagementbe.record.dto;

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
public class SortCriteriaDto {
    @NotBlank(message = "Column name is required")
    private String columnName;

    @Pattern(regexp = "^(ASC|DESC)$", message = "Direction must be ASC or DESC")
    @Builder.Default
    private String direction = "ASC";
}
