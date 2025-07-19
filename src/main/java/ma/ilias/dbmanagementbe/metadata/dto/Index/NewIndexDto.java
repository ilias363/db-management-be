package ma.ilias.dbmanagementbe.metadata.dto.Index;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.Index.indexcolumn.NewIndexColumnDto;

import java.util.ArrayList;
import java.util.List;

@Data
public class NewIndexDto {
    @NotBlank(message = "Index name cannot be blank")
    private String indexName;

    @NotBlank(message = "Schema name cannot be blank")
    private String schemaName;

    @NotBlank(message = "Table name cannot be blank")
    private String tableName;

    private Boolean isUnique;

    @Pattern(regexp = "^(?i)(BTREE|HASH)$", message = "Index type is not valid")
    private String indexType;

    @Valid
    @NotEmpty(message = "At least one index column is required")
    private List<NewIndexColumnDto> indexColumns = new ArrayList<>();
}
