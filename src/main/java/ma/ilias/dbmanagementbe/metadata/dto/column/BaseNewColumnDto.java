package ma.ilias.dbmanagementbe.metadata.dto.column;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.NewPrimaryKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykeyforeignkey.NewPrimaryKeyForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.NewStandardColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.common.ColumnDataTypeDefinition;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnReference;
import ma.ilias.dbmanagementbe.validation.annotations.RequiredColumnDefault;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueColumnName;
import ma.ilias.dbmanagementbe.validation.annotations.ValidColumnDefault;
import ma.ilias.dbmanagementbe.validation.annotations.ValidDataTypeDefinition;
import ma.ilias.dbmanagementbe.validation.groups.StandaloneColumnCreation;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "columnType"
)
@JsonSubTypes({
    @JsonSubTypes.Type(value = NewStandardColumnDto.class, name = "STANDARD"),
    @JsonSubTypes.Type(value = NewPrimaryKeyColumnDto.class, name = "PRIMARY_KEY"),
    @JsonSubTypes.Type(value = NewForeignKeyColumnDto.class, name = "FOREIGN_KEY"),
    @JsonSubTypes.Type(value = NewPrimaryKeyForeignKeyColumnDto.class, name = "PRIMARY_KEY_FOREIGN_KEY")
})
@ValidDataTypeDefinition
@ValidColumnDefault
@RequiredColumnDefault(groups = StandaloneColumnCreation.class)
@UniqueColumnName(groups = StandaloneColumnCreation.class)
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

    public abstract ColumnType getColumnType();
}
