package ma.ilias.dbmanagementbe.metadata.dto.foreignkey;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableMetadataDto;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class ForeignKeyMetadataDto {
    private String constraintName;

    @JsonIgnoreProperties({"columns", "indexes", "foreignKeys"})
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private TableMetadataDto fromTable;
    private String fromColumnName;

    private String toTableName;
    private String toColumnName;

    private String onDeleteAction; // e.g., CASCADE, SET NULL, NO ACTION
    private String onUpdateAction; // same options
}
