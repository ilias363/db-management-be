package ma.ilias.dbmanagementbe.metadata.dto.table;

import java.util.List;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.common.ITableReference;
import ma.ilias.dbmanagementbe.validation.annotations.NoDuplicateColumnNames;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueTableName;

@Data
@UniqueTableName
@NoDuplicateColumnNames
public class NewTableDto implements ITableReference {
    @NotBlank(message = "Schema name cannot be blank")
    // schema existence checked in @UniqueTableName
    private String schemaName;

    @NotBlank(message = "Table name cannot be blank")
    @Pattern(
        regexp = "^[a-zA-Z][a-zA-Z0-9_]*$",
        message = "Table name must start with a letter and contain only alphanumeric characters and underscores")
    private String tableName;

    @Valid
    @NotEmpty(message = "At least one column is required")
    private List<BaseNewColumnDto> columns;
}
