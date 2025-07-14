package ma.ilias.dbmanagementbe.metadata.dto.table;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.ITableReference;
import ma.ilias.dbmanagementbe.metadata.dto.column.NewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.foreignkey.NewForeignKeyDto;
import ma.ilias.dbmanagementbe.validation.UniqueTableName;

import java.util.List;

@Data
@UniqueTableName
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
    @NotNull
    private NewColumnDto primaryKey;

    @Valid
    @NotEmpty(message = "At least one column is required")
    private List<NewColumnDto> columns;

    @Valid
    private List<NewForeignKeyDto> foreignKeys;
}
