package ma.ilias.dbmanagementbe.metadata.dto.column;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.metadata.dto.IColumnReference;
import ma.ilias.dbmanagementbe.validation.NotNullOrUnique;
import ma.ilias.dbmanagementbe.validation.UniqueColumnName;
import ma.ilias.dbmanagementbe.validation.ValidColumnDefault;
import ma.ilias.dbmanagementbe.validation.groups.StandaloneColumnCreation;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ValidColumnDefault
@UniqueColumnName(groups = StandaloneColumnCreation.class)
@NotNullOrUnique(groups = StandaloneColumnCreation.class)
public abstract class BaseNewColumnDto implements ColumnDataTypeDefinition, IColumnReference {
    @NotBlank(
            message = "Schema name cannot be blank",
            groups = StandaloneColumnCreation.class
    )
    // schemaName existence is checked in @UniqueColumnName
    private String schemaName;

    @NotBlank(
            message = "Table name cannot be blank",
            groups = StandaloneColumnCreation.class
    )
    // tableName existence is checked in @UniqueColumnName
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
