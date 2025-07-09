package ma.ilias.dbmanagementbe.metadata.dto.Index;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.metadata.dto.indexcolumn.NewIndexColumnDto;

import java.util.ArrayList;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class NewIndexDto {
    @NotBlank(message = "Index name cannot be blank")
    private String indexName;

    @NotBlank(message = "Table name cannot be blank")
    private String tableName;

    private Boolean isUnique;

    private String indexType;

    @Valid
    @NotEmpty(message = "At least one index column is required")
    private List<NewIndexColumnDto> indexColumns = new ArrayList<>();
}
