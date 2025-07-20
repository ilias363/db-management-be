package ma.ilias.dbmanagementbe.metadata.dto.column.update;

import java.util.List;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingColumns;
import ma.ilias.dbmanagementbe.validation.annotations.NotSystemSchema;
import ma.ilias.dbmanagementbe.validation.annotations.ValidPrimaryKeyChange;

@Data
@ExistingColumns
@ValidPrimaryKeyChange
public class UpdateColumnPrimaryKeyDto {
    @NotBlank(message = "Schema name is required")
    @NotSystemSchema
    private String schemaName;

    @NotBlank(message = "Table name is required")
    private String tableName;

    private List<String> columnNames;

    @NotNull(message = "Primary key value is required")
    private Boolean isPrimaryKey;
}
