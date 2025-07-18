package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnPrimaryKeyDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;

@RequiredArgsConstructor
public class ValidPrimaryKeyChangeValidator implements ConstraintValidator<ValidPrimaryKeyChange, UpdateColumnPrimaryKeyDto> {

    private final ColumnService columnService;

    @Override
    public boolean isValid(UpdateColumnPrimaryKeyDto dto, ConstraintValidatorContext context) {
        if (dto == null || dto.getIsPrimaryKey() == null) {
            return true;
        }

        try {
            var currentColumn = columnService.getColumn(
                    dto.getSchemaName(),
                    dto.getTableName(),
                    dto.getColumnName()
            );

            boolean isCurrentlyPrimaryKey = currentColumn.getColumnType() == ColumnType.PRIMARY_KEY;
            if (isCurrentlyPrimaryKey == dto.getIsPrimaryKey()) {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate("Column is already " +
                                (isCurrentlyPrimaryKey ? "a primary key" : "not a primary key"))
                        .addConstraintViolation();
                return false;
            }

            if (dto.getIsPrimaryKey() && Boolean.TRUE.equals(currentColumn.getIsNullable())) {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate("Cannot add primary key constraint to nullable column. Make column NOT NULL first.")
                        .addConstraintViolation();
                return false;
            }

            // If removing PK constraint, check if there are referencing foreign keys
            // It will be checked in the service layer for referencing constraints
        } catch (Exception e) {
            return true;
        }

        return true;
    }
}
