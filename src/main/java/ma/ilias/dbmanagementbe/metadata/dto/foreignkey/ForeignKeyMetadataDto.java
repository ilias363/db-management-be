package ma.ilias.dbmanagementbe.metadata.dto.foreignkey;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.metadata.dto.column.ColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class ForeignKeyMetadataDto {
    private String constraintName;
    private String onDeleteAction; // e.g., CASCADE, SET NULL, NO ACTION
    private String onUpdateAction; // same options

    @JsonIgnoreProperties({"columns", "indexes", "foreignKeys"})
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private TableMetadataDto table;

    @JsonIgnoreProperties("table")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private ColumnMetadataDto fromColumn;

    @JsonIgnoreProperties("table")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private ColumnMetadataDto toColumn;
}
