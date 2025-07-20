package ma.ilias.dbmanagementbe.validation.validators;

import java.util.Set;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.NewPrimaryKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.NewTableDto;
import ma.ilias.dbmanagementbe.validation.annotations.NoAutoIncrement;

public class NoAutoIncrementValidator implements ConstraintValidator<NoAutoIncrement, NewTableDto> {

    @Override
    public boolean isValid(NewTableDto newTableDto, ConstraintValidatorContext context) {
        if (newTableDto.getColumns() == null || newTableDto.getColumns().isEmpty()) {
            return true;
        }

        long primaryKeyCount = newTableDto.getColumns().stream()
                .filter(c -> Set.of(ColumnType.PRIMARY_KEY, ColumnType.PRIMARY_KEY_FOREIGN_KEY)
                        .contains(c.getColumnType()))
                .count();

        if (primaryKeyCount > 1) {
            boolean hasAutoIncrement = newTableDto.getColumns().stream()
                    .anyMatch(c -> c instanceof NewPrimaryKeyColumnDto
                            && ((NewPrimaryKeyColumnDto) c).getAutoIncrement() != null
                            && ((NewPrimaryKeyColumnDto) c).getAutoIncrement());

            if (primaryKeyCount > 1 && hasAutoIncrement) {
                return false;
            }
        }

        return true;
    }
}
