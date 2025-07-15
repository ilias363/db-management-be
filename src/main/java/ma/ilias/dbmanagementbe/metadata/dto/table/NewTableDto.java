package ma.ilias.dbmanagementbe.metadata.dto.table;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.ITableReference;
import ma.ilias.dbmanagementbe.metadata.dto.column.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.NewPrimaryKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.NewStandardColumnDto;
import ma.ilias.dbmanagementbe.validation.NoDuplicateColumnNames;
import ma.ilias.dbmanagementbe.validation.UniqueTableName;

import java.util.List;

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
            message = "Table name must start with a letter and contain only alphanumeric characters and underscores"
    )
    private String tableName;

    @Valid
    @NotNull(message = "Primary key is required")
    private NewPrimaryKeyColumnDto primaryKey;

    @Valid
    @NotEmpty(message = "At least one column is required")
    private List<NewStandardColumnDto> columns;

    @Valid
    private List<NewForeignKeyColumnDto> foreignKeyColumns;
}
