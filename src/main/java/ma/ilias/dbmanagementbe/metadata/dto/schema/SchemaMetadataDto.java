package ma.ilias.dbmanagementbe.metadata.dto.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class SchemaMetadataDto {
    private String schemaName;
    private LocalDateTime creationDate;

    @JsonIgnoreProperties({"schema", "columns", "indexes", "foreignKeys"})
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private List<TableMetadataDto> tables = new ArrayList<>();
}
