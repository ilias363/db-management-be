package ma.ilias.dbmanagementbe.metadata.dto.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class SchemaListResponseDto {
    @JsonIgnoreProperties({"tables", "views"})
    private List<SchemaMetadataDto> schemas;
}
