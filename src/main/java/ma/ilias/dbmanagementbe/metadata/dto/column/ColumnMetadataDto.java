package ma.ilias.dbmanagementbe.metadata.dto.column;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class ColumnMetadataDto {
    private String columnName;
    private Integer ordinalPosition;
    private String dataType;
    private Integer characterMaxLength;
    private Integer numericPrecision;
    private Integer numericScale;
    private Boolean isNullable;
    private Boolean isUnique;
    private String columnDefault;
    private Boolean isPrimaryKey;
    private Boolean autoIncrement;

    @JsonIgnoreProperties({"columns", "indexes", "foreignKeys"})
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private TableMetadataDto table;
}
