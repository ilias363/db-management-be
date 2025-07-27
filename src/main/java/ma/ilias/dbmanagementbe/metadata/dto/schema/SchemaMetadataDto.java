package ma.ilias.dbmanagementbe.metadata.dto.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.view.ViewMetadataDto;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class SchemaMetadataDto {
    private String schemaName;
    private Boolean isSystemSchema;
    private LocalDateTime creationDate;

    @JsonIgnoreProperties({"schema", "columns", "indexes"})
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    @Builder.Default
    private List<TableMetadataDto> tables = new ArrayList<>();

    @JsonIgnoreProperties({"schema", "columns"})
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    @Builder.Default
    private List<ViewMetadataDto> views = new ArrayList<>();
}
