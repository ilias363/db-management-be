package ma.ilias.dbmanagementbe.metadata.dto.view;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;

@Data
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public class ViewColumnMetadataDto extends BaseColumnMetadataDto {
    @JsonIgnoreProperties({"columns"})
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private ViewMetadataDto view;
}
