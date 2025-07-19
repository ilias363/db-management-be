package ma.ilias.dbmanagementbe.metadata.dto.index;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.enums.IndexType;
import ma.ilias.dbmanagementbe.metadata.dto.index.indexcolumn.IndexColumnMetadataDto;
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
    private IndexType indexType;

    @JsonIgnoreProperties({ "columns", "indexes" })
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private TableMetadataDto table;

    @JsonIgnoreProperties("index")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    @Builder.Default
    private List<IndexColumnMetadataDto> indexColumns = new ArrayList<>();
}
