package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnUniqueDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ValidUniqueChange;

@RequiredArgsConstructor
public class ValidUniqueChangeValidator implements ConstraintValidator<ValidUniqueChange, UpdateColumnUniqueDto> {

    private final MetadataProviderService metadataProviderService;

    @Override
    public boolean isValid(UpdateColumnUniqueDto dto, ConstraintValidatorContext context) {
        if (dto == null || dto.getIsUnique() == null) {
            return true;
        }

        try {
            var currentColumn = metadataProviderService.getColumn(
                    dto.getSchemaName(),
                    dto.getTableName(),
                    dto.getColumnName(),
                    false, false);
            if (currentColumn.getColumnType() != ColumnType.STANDARD) {
                ValidationUtils.addConstraintViolation(context,
                        "Is unique can only be used for standard columns",
                        null);
                return false;
            }
        } catch (Exception e) {
            return true;
        }

        return true;
    }
}
