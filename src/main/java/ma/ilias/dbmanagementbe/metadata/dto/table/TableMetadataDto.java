package ma.ilias.dbmanagementbe.metadata.dto.table;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;

import java.util.ArrayList;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class TableMetadataDto {
    private String tableName;
    private Integer columnCount;
    private Long rowCount;
    private Long sizeInBytes;

    @JsonIgnoreProperties({"tables", "views"})
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private SchemaMetadataDto schema;

    @JsonIgnoreProperties("table")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    @Builder.Default
    private List<BaseColumnMetadataDto> columns = new ArrayList<>();

    @JsonIgnoreProperties("table")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    @Builder.Default
    private List<IndexMetadataDto> indexes = new ArrayList<>();
}
