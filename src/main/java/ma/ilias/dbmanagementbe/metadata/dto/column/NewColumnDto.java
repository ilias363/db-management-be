package ma.ilias.dbmanagementbe.metadata.dto.column;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import ma.ilias.dbmanagementbe.validation.ValidColumnDefinition;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
@ValidColumnDefinition
public class NewColumnDto implements ColumnDataTypeDefinition {
    @NotBlank(message = "Schema name cannot be blank")
    private String schemaName;

    @NotBlank(message = "Table name cannot be blank")
    private String tableName;

    @NotBlank(message = "Column name cannot be blank")
    private String columnName;

    @NotBlank(message = "Data type cannot be blank")
    @Pattern(
            regexp = "^(?i)(VARCHAR|CHAR|TEXT|INT|INTEGER|SMALLINT|BIGINT|DECIMAL|NUMERIC|FLOAT|REAL|DOUBLE|BOOLEAN|DATE|TIME|TIMESTAMP)$",
            message = "Invalid data type"
    )
    private String dataType;

    private Integer characterMaxLength;
    private Integer numericPrecision;
    private Integer numericScale;

    private Boolean isNullable;
    private Boolean isUnique;
    private String columnDefault;
    private Boolean autoIncrement;
}

