package ma.ilias.dbmanagementbe.record.dto;

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
public class RecordPageDto {
    @JsonIgnoreProperties({"schemaName", "tableName"})
    private List<RecordDto> records;
    private long totalRecords;
    private int currentPage;
    private int pageSize;
    private int totalPages;
    private String tableName;
    private String schemaName;
}
