package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.NewStandardColumnDto;

public class RequiredColumnDefaultValidator implements ConstraintValidator<RequiredColumnDefault, BaseNewColumnDto> {

    @Override
    public boolean isValid(BaseNewColumnDto dto, ConstraintValidatorContext context) {
        if (dto == null) {
            return true;
        }

        if (dto instanceof NewStandardColumnDto standardCol) {
            return standardCol.getIsNullable() == null
                    || standardCol.getIsNullable()
                    || standardCol.getColumnDefault() == null
                    || !standardCol.getColumnDefault().isBlank();
        }

        if (dto instanceof NewForeignKeyColumnDto fkCol) {
            return fkCol.getIsNullable() == null
                    || fkCol.getIsNullable()
                    || fkCol.getColumnDefault() == null
                    || !fkCol.getColumnDefault().isBlank();
        }

        return true;
    }
}
