package ma.ilias.dbmanagementbe.metadata.dto.table;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.metadata.dto.Index.IndexMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.ColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.foreignkey.ForeignKeyMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class TableMetadataDto {
    private String tableName;
    private Boolean isSystemTable;
    private Integer columnCount;
    private Long rowCount;
    private Long sizeInBytes;
    private LocalDateTime lastModified;

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

    @JsonIgnoreProperties("table")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private List<ForeignKeyMetadataDto> foreignKeys = new ArrayList<>();
}
