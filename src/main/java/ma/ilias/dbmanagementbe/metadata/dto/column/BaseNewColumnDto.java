package ma.ilias.dbmanagementbe.metadata.dto.column;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.validation.ValidColumnDefault;

@Data
@SuperBuilder
@ValidColumnDefault
public abstract class BaseNewColumnDto implements ColumnDataTypeDefinition {
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

    public BaseNewColumnDto() {
        super();
    }

    public BaseNewColumnDto(String columnName, String dataType, Integer characterMaxLength,
                            Integer numericPrecision, Integer numericScale, Boolean isNullable,
                            Boolean isUnique, String columnDefault, Boolean autoIncrement) {
        this.columnName = columnName;
        this.dataType = dataType;
        this.characterMaxLength = characterMaxLength;
        this.numericPrecision = numericPrecision;
        this.numericScale = numericScale;
        this.isNullable = isNullable;
        this.isUnique = isUnique;
        this.columnDefault = columnDefault;
        this.autoIncrement = autoIncrement;
    }
}
