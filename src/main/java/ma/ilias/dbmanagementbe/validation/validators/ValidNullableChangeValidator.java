package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnNullableDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ValidNullableChange;

import java.util.Set;

@RequiredArgsConstructor
public class ValidNullableChangeValidator implements ConstraintValidator<ValidNullableChange, UpdateColumnNullableDto> {

    private final MetadataProviderService metadataProviderService;

    @Override
    public boolean isValid(UpdateColumnNullableDto dto, ConstraintValidatorContext context) {
        if (dto == null || dto.getIsNullable() == null) {
            return true;
        }

        try {
            var currentColumn = metadataProviderService.getColumn(
                    dto.getSchemaName(),
                    dto.getTableName(),
                    dto.getColumnName(),
                    false, false);
            if (Set.of(ColumnType.PRIMARY_KEY, ColumnType.PRIMARY_KEY_FOREIGN_KEY)
                    .contains(currentColumn.getColumnType())) {
                ValidationUtils.addConstraintViolation(context,
                        "Is nullable can only be used for standard or foreign key columns",
                        null);
                return false;
            }
        } catch (Exception e) {
            return true;
        }

        return true;
    }
}
