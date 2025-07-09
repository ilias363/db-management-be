package ma.ilias.dbmanagementbe.metadata.dto.indexcolumn;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.metadata.dto.Index.IndexMetadataDto;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class IndexColumnMetadataDto {
    private String columnName;
    private Integer ordinalPosition;
    private String sortOrder; // ASC, DESC, null

    @JsonIgnoreProperties("indexColumns")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private IndexMetadataDto index;
}
