package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnUniqueDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.annotations.ValidUniqueChange;

@RequiredArgsConstructor
public class ValidUniqueChangeValidator implements ConstraintValidator<ValidUniqueChange, UpdateColumnUniqueDto> {

    private final ColumnService columnService;

    @Override
    public boolean isValid(UpdateColumnUniqueDto dto, ConstraintValidatorContext context) {
        if (dto == null || dto.getIsUnique() == null) {
            return true;
        }

        try {
            var currentColumn = columnService.getColumn(
                    dto.getSchemaName(),
                    dto.getTableName(),
                    dto.getColumnName()
            );
            if (currentColumn.getColumnType() != ColumnType.STANDARD) {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate("Is unique can only be used for standard columns")
                        .addConstraintViolation();
                return false;
            }
        } catch (Exception e) {
            return true;
        }

        return true;
    }
}
