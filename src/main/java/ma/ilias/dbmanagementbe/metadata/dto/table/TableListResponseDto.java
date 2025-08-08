package ma.ilias.dbmanagementbe.metadata.dto.table;

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
public class TableListResponseDto {
    @JsonIgnoreProperties({"schema", "columns", "indexes"})
    List<TableMetadataDto> tables;
}
