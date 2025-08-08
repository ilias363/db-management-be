package ma.ilias.dbmanagementbe.metadata.dto.view;

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
public class ViewListResponseDto {
    @JsonIgnoreProperties({"schema", "columns"})
    List<ViewMetadataDto> views;
}
