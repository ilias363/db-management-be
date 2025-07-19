package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnForeignKeyDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ValidForeignKeyChange;

@RequiredArgsConstructor
public class ValidForeignKeyChangeValidator
        implements ConstraintValidator<ValidForeignKeyChange, UpdateColumnForeignKeyDto> {

    private final ColumnService columnService;

    @Override
    public boolean isValid(UpdateColumnForeignKeyDto dto, ConstraintValidatorContext context) {
        if (dto == null || dto.getIsForeignKey() == null) {
            return true;
        }

        context.disableDefaultConstraintViolation();

        try {
            var currentColumn = columnService.getColumn(
                    dto.getSchemaName(),
                    dto.getTableName(),
                    dto.getColumnName());

            boolean isCurrentlyForeignKey = currentColumn.getColumnType() == ColumnType.FOREIGN_KEY;
            if (isCurrentlyForeignKey == dto.getIsForeignKey()) {
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Column is already " + (isCurrentlyForeignKey ? "a foreign key" : "not a foreign key"),
                        null);
                return false;
            }

            if (dto.getIsForeignKey()) {
                return validateForeignKeyConstraints(dto, currentColumn, context);
            }

        } catch (Exception e) {
            return true;
        }

        return true;
    }

    private boolean validateForeignKeyConstraints(UpdateColumnForeignKeyDto dto, BaseColumnMetadataDto currentColumn,
            ConstraintValidatorContext context) {
        boolean isValid = true;

        isValid &= ValidationUtils.validateRequiredStringField(dto.getReferencedSchemaName(), context,
                "Referenced schema name is required when adding foreign key constraint", "referencedSchemaName");

        isValid &= ValidationUtils.validateRequiredStringField(dto.getReferencedTableName(), context,
                "Referenced table name is required when adding foreign key constraint", "referencedTableName");

        isValid &= ValidationUtils.validateRequiredStringField(dto.getReferencedColumnName(), context,
                "Referenced column name is required when adding foreign key constraint", "referencedColumnName");

        if (!isValid) {
            return false;
        }

        try {
            var referencedColumn = columnService.getColumn(
                    dto.getReferencedSchemaName(),
                    dto.getReferencedTableName(),
                    dto.getReferencedColumnName());

            isValid &= ValidationUtils.validateDataTypeMatch(currentColumn.getDataType(),
                    referencedColumn.getDataType(), context, null);

            if (isValid) {
                isValid &= validateDataTypeProperties(currentColumn, referencedColumn, context);
            }

        } catch (Exception e) {
            // Referenced column not found - let other validators handle this
        }

        return isValid;
    }

    private boolean validateDataTypeProperties(BaseColumnMetadataDto currentColumn,
            BaseColumnMetadataDto referencedColumn,
            ConstraintValidatorContext context) {
        String dataType = currentColumn.getDataType().toUpperCase();

        return switch (dataType) {
            case "VARCHAR", "CHAR" -> ValidationUtils.validateCharacterPropertiesMatch(
                    currentColumn.getCharacterMaxLength(),
                    referencedColumn.getCharacterMaxLength(),
                    context);
            case "DECIMAL", "NUMERIC" -> ValidationUtils.validateNumericPropertiesMatch(
                    currentColumn.getNumericPrecision(),
                    referencedColumn.getNumericPrecision(),
                    currentColumn.getNumericScale(),
                    referencedColumn.getNumericScale(),
                    context);
            default -> true;
        };
    }
}
