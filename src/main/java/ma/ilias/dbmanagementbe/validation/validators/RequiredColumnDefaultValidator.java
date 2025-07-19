package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.NewStandardColumnDto;
import ma.ilias.dbmanagementbe.validation.annotations.RequiredColumnDefault;

public class RequiredColumnDefaultValidator implements ConstraintValidator<RequiredColumnDefault, BaseNewColumnDto> {

    @Override
    public boolean isValid(BaseNewColumnDto dto, ConstraintValidatorContext context) {
        if (dto == null) {
            return true;
        }

        if (dto instanceof NewStandardColumnDto standardCol) {
            return validateColumnDefault(standardCol.getIsNullable(), standardCol.getColumnDefault());
        }

        if (dto instanceof NewForeignKeyColumnDto fkCol) {
            return validateColumnDefault(fkCol.getIsNullable(), fkCol.getColumnDefault());
        }

        return true;
    }

    private boolean validateColumnDefault(Boolean isNullable, String columnDefault) {
        return isNullable == null
                || isNullable
                || columnDefault == null
                || !columnDefault.isBlank();
    }
}
