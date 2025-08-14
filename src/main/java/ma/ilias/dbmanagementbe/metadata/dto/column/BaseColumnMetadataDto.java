package ma.ilias.dbmanagementbe.metadata.dto.column;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnDataTypeDefinition;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public abstract class BaseColumnMetadataDto implements IColumnDataTypeDefinition {
    private String columnName;
    private Integer ordinalPosition;
    private String dataType;
    private Long characterMaxLength;
    private Integer numericPrecision;
    private Integer numericScale;
    private Boolean isNullable;
    private Boolean isUnique;
    private String columnDefault;
    private Boolean autoIncrement;
}
