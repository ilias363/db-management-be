package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.table.NewTableDto;

import java.util.HashSet;
import java.util.Set;

public class NoDuplicateColumnNamesValidator implements ConstraintValidator<NoDuplicateColumnNames, NewTableDto> {
    @Override
    public boolean isValid(NewTableDto dto, ConstraintValidatorContext context) {
        Set<String> names = new HashSet<>();
        boolean valid = true;
        context.disableDefaultConstraintViolation();

        if (dto.getPrimaryKey() != null && dto.getPrimaryKey().getColumnName() != null) {
            names.add(dto.getPrimaryKey().getColumnName());
        }

        if (dto.getColumns() != null) {
            for (int i = 0; i < dto.getColumns().size(); i++) {
                var col = dto.getColumns().get(i);
                if (col.getColumnName() != null && !names.add(col.getColumnName())) {
                    context.buildConstraintViolationWithTemplate("Duplicate column name: " + col.getColumnName())
                            .addPropertyNode("columns[" + i + "].columnName").addConstraintViolation();
                    valid = false;
                }
            }
        }

        if (dto.getForeignKeyColumns() != null) {
            for (int i = 0; i < dto.getForeignKeyColumns().size(); i++) {
                var fk = dto.getForeignKeyColumns().get(i);
                if (fk.getColumnName() != null && !names.add(fk.getColumnName())) {
                    context.buildConstraintViolationWithTemplate("Duplicate column name: " + fk.getColumnName())
                            .addPropertyNode("foreignKeyColumns[" + i + "].columnName").addConstraintViolation();
                    valid = false;
                }
            }
        }
        return valid;
    }
}
