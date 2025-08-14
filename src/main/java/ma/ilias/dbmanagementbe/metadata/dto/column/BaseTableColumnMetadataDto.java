package ma.ilias.dbmanagementbe.metadata.dto.column;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import lombok.experimental.SuperBuilder;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;

@Data
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public abstract class BaseTableColumnMetadataDto extends BaseColumnMetadataDto {
    @JsonIgnoreProperties({"columns", "indexes"})
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private TableMetadataDto table;

    public abstract ColumnType getColumnType();
}
