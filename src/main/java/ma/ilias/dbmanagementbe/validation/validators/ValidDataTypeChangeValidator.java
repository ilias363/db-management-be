package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnDataTypeDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ValidDataTypeChange;

@RequiredArgsConstructor
public class ValidDataTypeChangeValidator implements ConstraintValidator<ValidDataTypeChange, UpdateColumnDataTypeDto> {

    private final ColumnService columnService;

    @Override
    public boolean isValid(UpdateColumnDataTypeDto value, ConstraintValidatorContext context) {
        if (value == null || value.getDataType() == null) {
            return true;
        }

        try {
            BaseColumnMetadataDto currentColumn = columnService.getColumn(
                    value.getSchemaName(),
                    value.getTableName(),
                    value.getColumnName());

            if (currentColumn == null) {
                return true;
            }

            return ValidationUtils.validateDataTypeChangeCompatibility(
                    currentColumn.getDataType(),
                    value.getDataType(),
                    context);

        } catch (Exception e) {
            return true;
        }
    }
}
