package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseTableColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnDataTypeDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ValidDataTypeChange;

@RequiredArgsConstructor
public class ValidDataTypeChangeValidator implements ConstraintValidator<ValidDataTypeChange, UpdateColumnDataTypeDto> {

    private final MetadataProviderService metadataProviderService;

    @Override
    public boolean isValid(UpdateColumnDataTypeDto value, ConstraintValidatorContext context) {
        if (value == null || value.getDataType() == null) {
            return true;
        }

        try {
            BaseTableColumnMetadataDto currentColumn = metadataProviderService.getColumn(
                    value.getSchemaName(),
                    value.getTableName(),
                    value.getColumnName(),
                    false, false);

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
