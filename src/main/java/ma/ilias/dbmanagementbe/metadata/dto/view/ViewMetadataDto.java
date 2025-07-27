package ma.ilias.dbmanagementbe.metadata.dto.view;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.schema.SchemaMetadataDto;

import java.util.ArrayList;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class ViewMetadataDto {
    private String viewName;
    private String viewDefinition;
    private String checkOption;
    private Boolean isUpdatable;
    private String characterSet;
    private String collation;

    @JsonIgnoreProperties("tables")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private SchemaMetadataDto schema;

    @JsonIgnoreProperties("table")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    @Builder.Default
    private List<BaseColumnMetadataDto> columns = new ArrayList<>();
}
