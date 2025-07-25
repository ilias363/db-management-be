package ma.ilias.dbmanagementbe.record.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.dto.PageDto;

@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class RecordPageDto extends PageDto<RecordDto> {
    private String tableName;
    private String schemaName;

    @JsonIgnoreProperties({"schemaName", "tableName"})
    public java.util.List<RecordDto> getItems() {
        return super.getItems();
    }
}
