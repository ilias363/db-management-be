package ma.ilias.dbmanagementbe.metadata.dto.table;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.metadata.dto.Index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.ColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.foreignkey.ForeignKeyMetadataDto;
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

    @JsonIgnoreProperties("tables")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private SchemaMetadataDto schema;

    @JsonIgnoreProperties("table")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private List<ColumnMetadataDto> columns = new ArrayList<>();

    @JsonIgnoreProperties("table")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private List<IndexMetadataDto> indexes = new ArrayList<>();

    @JsonIgnoreProperties("fromTable")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private List<ForeignKeyMetadataDto> foreignKeys = new ArrayList<>();
}
