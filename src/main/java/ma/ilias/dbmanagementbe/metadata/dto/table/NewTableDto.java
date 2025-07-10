package ma.ilias.dbmanagementbe.metadata.dto.table;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.metadata.dto.column.NewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.foreignkey.NewForeignKeyDto;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class NewTableDto {
    @NotBlank(message = "Schema name cannot be blank")
    private String schemaName;

    @NotBlank(message = "Table name cannot be blank")
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
