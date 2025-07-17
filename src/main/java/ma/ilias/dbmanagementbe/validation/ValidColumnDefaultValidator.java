package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.NewStandardColumnDto;

public class ValidColumnDefaultValidator implements ConstraintValidator<ValidColumnDefault, BaseNewColumnDto> {

    private final ValidStandardColumnDefaultValidator standardValidator = new ValidStandardColumnDefaultValidator();
    private final ValidForeignKeyColumnDefaultValidator foreignKeyValidator = new ValidForeignKeyColumnDefaultValidator();

    @Override
    public boolean isValid(BaseNewColumnDto dto, ConstraintValidatorContext context) {
        if (dto == null) {
            return true;
        }

        if (dto instanceof NewStandardColumnDto standardDto) {
            return standardValidator.isValid(standardDto, context);
        } else if (dto instanceof NewForeignKeyColumnDto foreignKeyDto) {
            return foreignKeyValidator.isValid(foreignKeyDto, context);
        }

        return true;
    }
}
