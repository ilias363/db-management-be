package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.table.NewTableDto;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.NoDuplicateColumnNames;

import java.util.HashSet;
import java.util.Set;

public class NoDuplicateColumnNamesValidator implements ConstraintValidator<NoDuplicateColumnNames, NewTableDto> {
    @Override
    public boolean isValid(NewTableDto dto, ConstraintValidatorContext context) {
        Set<String> names = new HashSet<>();
        boolean valid = true;

        if (dto.getColumns() != null) {
            for (int i = 0; i < dto.getColumns().size(); i++) {
                var col = dto.getColumns().get(i);
                if (col.getColumnName() != null && !names.add(col.getColumnName())) {
                    ValidationUtils.addConstraintViolation(context,
                            "Duplicate column name: " + col.getColumnName(),
                            "columns[" + i + "].columnName");
                    valid = false;
                }
            }
        }

        return valid;
    }
}
