package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnNullableDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.annotations.ValidNullableChange;

@RequiredArgsConstructor
public class ValidNullableChangeValidator implements ConstraintValidator<ValidNullableChange, UpdateColumnNullableDto> {

    private final ColumnService columnService;

    @Override
    public boolean isValid(UpdateColumnNullableDto dto, ConstraintValidatorContext context) {
        if (dto == null || dto.getIsNullable() == null) {
            return true;
        }

        try {
            var currentColumn = columnService.getColumn(
                    dto.getSchemaName(),
                    dto.getTableName(),
                    dto.getColumnName()
            );
            if (currentColumn.getColumnType() == ColumnType.PRIMARY_KEY) {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate("Is nullable can only be used for standard or foreign key columns")
                        .addConstraintViolation();
                return false;
            }
        } catch (Exception e) {
            return true;
        }

        return true;
    }
}
