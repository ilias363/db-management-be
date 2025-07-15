package ma.ilias.dbmanagementbe.metadata.dto.column;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public abstract class BaseColumnMetadataDto {
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

    @JsonIgnoreProperties({"columns", "indexes"})
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private TableMetadataDto table;

    public abstract ColumnType getColumnType();
}
