package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseTableColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.MatchingForeignKeyType;

@RequiredArgsConstructor
public class MatchingForeignKeyTypeValidator
        implements ConstraintValidator<MatchingForeignKeyType, BaseNewForeignKeyColumnDto> {

    private final MetadataProviderService metadataProviderService;

    @Override
    public boolean isValid(BaseNewForeignKeyColumnDto dto, ConstraintValidatorContext context) {
        if (dto == null ||
                ValidationUtils.hasNullOrBlankValues(
                        dto.getReferencedSchemaName(),
                        dto.getReferencedTableName(),
                        dto.getReferencedColumnName(),
                        dto.getDataType())) {
            return true;
        }

        boolean isValid;
        context.disableDefaultConstraintViolation();

        try {
            BaseTableColumnMetadataDto referencedColumn = metadataProviderService.getColumn(
                    dto.getReferencedSchemaName(),
                    dto.getReferencedTableName(),
                    dto.getReferencedColumnName(),
                    false, false);

            // Check if data types match
            if (!ValidationUtils.validateDataTypeMatch(dto.getDataType(), referencedColumn.getDataType(), context,
                    "dataType")) {
                isValid = false;
            } else {
                // Only check additional properties if data types match
                isValid = validateDataTypeProperties(dto, referencedColumn, context);
            }

        } catch (Exception e) {
            return true;
        }

        return isValid;
    }

    private boolean validateDataTypeProperties(BaseNewForeignKeyColumnDto dto, BaseTableColumnMetadataDto referencedColumn,
                                               ConstraintValidatorContext context) {
        String dataType = dto.getDataType().toUpperCase();

        return switch (dataType) {
            case "VARCHAR", "CHAR" -> ValidationUtils.validateCharacterPropertiesMatch(
                    dto.getCharacterMaxLength() != null ? dto.getCharacterMaxLength().longValue() : null,
                    referencedColumn.getCharacterMaxLength(),
                    context);
            case "DECIMAL", "NUMERIC" -> ValidationUtils.validateNumericPropertiesMatch(
                    dto.getNumericPrecision(),
                    referencedColumn.getNumericPrecision(),
                    dto.getNumericScale(),
                    referencedColumn.getNumericScale(),
                    context);
            default -> true;
        };
    }
}
