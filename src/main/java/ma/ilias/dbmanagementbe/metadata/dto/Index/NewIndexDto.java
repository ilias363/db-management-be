package ma.ilias.dbmanagementbe.metadata.dto.index;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.common.ITableReference;
import ma.ilias.dbmanagementbe.metadata.dto.index.indexcolumn.NewIndexColumnDto;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingTable;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueIndexName;
import ma.ilias.dbmanagementbe.validation.annotations.ValidIndexType;

import java.util.ArrayList;
import java.util.List;

@Data
@ExistingTable
@UniqueIndexName
public class NewIndexDto implements ITableReference {
    @NotBlank(message = "Index name cannot be blank")
    private String indexName;

    @NotBlank(message = "Schema name cannot be blank")
    private String schemaName;

    @NotBlank(message = "Table name cannot be blank")
    private String tableName;

    private Boolean isUnique;

    @NotNull(message = "Index type cannot be null")
    @ValidIndexType
    private String indexType;

    @Valid
    @NotEmpty(message = "At least one index column is required")
    private List<NewIndexColumnDto> indexColumns = new ArrayList<>();
}
