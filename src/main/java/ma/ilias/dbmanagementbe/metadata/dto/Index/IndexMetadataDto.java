package ma.ilias.dbmanagementbe.metadata.dto.Index;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.metadata.dto.indexcolumn.IndexColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;

import java.util.ArrayList;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class IndexMetadataDto {
    private String indexName;
    private Boolean isUnique;
    private String indexType;

    @JsonIgnoreProperties({"columns", "indexes", "foreignKeys"})
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private TableMetadataDto table;

    @JsonIgnoreProperties("index")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    @Builder.Default
    private List<IndexColumnMetadataDto> indexColumns = new ArrayList<>();
}
