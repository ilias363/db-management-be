package ma.ilias.dbmanagementbe.metadata.dto.column;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
public class NewForeignKeyColumnDto extends BaseNewColumnDto {
    @NotBlank(message = "Referenced schema name cannot be blank")
    private String referencedSchemaName;

    @NotBlank(message = "Referenced table name cannot be blank")
    private String referencedTableName;

    @NotBlank(message = "Referenced column name cannot be blank")
    private String referencedColumnName;

    @Pattern(
            regexp = "^(?i)(CASCADE|NO ACTION|RESTRICT|SET NULL|SET DEFAULT)$",
            message = "Delete action is not valid"
    )
    private String onDeleteAction;

    @Pattern(
            regexp = "^(?i)(CASCADE|NO ACTION|RESTRICT|SET NULL|SET DEFAULT)$",
            message = "Update action is not valid"
    )
    private String onUpdateAction;

    public NewForeignKeyColumnDto() {
        super();
    }

    public NewForeignKeyColumnDto(String columnName, String dataType, Integer characterMaxLength,
                                  Integer numericPrecision, Integer numericScale, Boolean isNullable,
                                  Boolean isUnique, String columnDefault, Boolean autoIncrement,
                                  String referencedSchemaName, String referencedTableName,
                                  String referencedColumnName, String onDeleteAction, String onUpdateAction) {
        super(columnName, dataType, characterMaxLength, numericPrecision, numericScale,
                isNullable, isUnique, columnDefault, autoIncrement);
        this.referencedSchemaName = referencedSchemaName;
        this.referencedTableName = referencedTableName;
        this.referencedColumnName = referencedColumnName;
        this.onDeleteAction = onDeleteAction;
        this.onUpdateAction = onUpdateAction;
    }
}
