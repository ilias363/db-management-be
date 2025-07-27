package ma.ilias.dbmanagementbe.metadata.dto.view;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ViewColumnMetadataDto {
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

    @JsonIgnoreProperties({"columns"})
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private ViewMetadataDto view;
}
