package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnDefaultDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ValidColumnDefaultUpdate;

import java.util.Objects;
import java.util.Set;

@RequiredArgsConstructor
public class ValidColumnDefaultUpdateValidator
        implements ConstraintValidator<ValidColumnDefaultUpdate, UpdateColumnDefaultDto> {

    private final ColumnService columnService;

    @Override
    public boolean isValid(UpdateColumnDefaultDto dto, ConstraintValidatorContext context) {
        if (dto == null || dto.getColumnDefault() == null) {
            return true;
        }

        BaseColumnMetadataDto currentColumn;

        try {
            currentColumn = columnService.getColumn(dto.getSchemaName(), dto.getTableName(), dto.getColumnName());
            if (currentColumn == null) {
                return true;
            }
        } catch (Exception e) {
            return true;
        }

        if (Set.of(ColumnType.PRIMARY_KEY, ColumnType.PRIMARY_KEY_FOREIGN_KEY)
                .contains(currentColumn.getColumnType())) {
            ValidationUtils.addConstraintViolation(context,
                    "Column default can only be used for standard or foreign key columns",
                    null);
            return false;
        }

        if (Objects.equals(currentColumn.getColumnDefault(), dto.getColumnDefault()))
            return true;

        String newDefaultValue = dto.getColumnDefault();
        String dataType = currentColumn.getDataType();
        Boolean isNullable = currentColumn.getIsNullable();
        Boolean isUnique = currentColumn.getIsUnique();
        Long charLen = currentColumn.getCharacterMaxLength();
        Integer numPrecision = currentColumn.getNumericPrecision();
        Integer numScale = currentColumn.getNumericScale();

        if (!newDefaultValue.equalsIgnoreCase("NULL") && isUnique != null && isUnique) {
            ValidationUtils.addConstraintViolation(context,
                    "If a column is unique, the default value should be null",
                    "columnDefault");
            return false;
        }

        if (isNullable != null && isNullable && "NULL".equalsIgnoreCase(newDefaultValue))
            return true;
        if (dataType == null)
            return true;

        context.disableDefaultConstraintViolation();

        return switch (dataType.toUpperCase()) {
            case "VARCHAR", "CHAR" -> {
                if (charLen == null || newDefaultValue.length() <= charLen) {
                    yield true;
                }
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Column default string length is bigger than the maximum allowed length",
                        "columnDefault");
                yield false;
            }
            case "TEXT" -> {
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Data type TEXT cannot have default",
                        "columnDefault");
                yield false;
            }
            case "INT", "INTEGER", "SMALLINT", "BIGINT" -> {
                if (ValidationUtils.validateIntegerValue(newDefaultValue))
                    yield true;
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Column default should be a valid integer",
                        "columnDefault");
                yield false;
            }
            case "DECIMAL", "NUMERIC" -> {
                if (numPrecision != null && numScale != null) {
                    if (!ValidationUtils.validateDecimalFormat(newDefaultValue, numPrecision, numScale)) {
                        ValidationUtils.addConstraintViolationKeepDefault(context,
                                "Decimal column default should have a valid format (precision, scale)",
                                "columnDefault");
                        yield false;
                    }
                }

                if (!ValidationUtils.validateDecimalValue(newDefaultValue)) {
                    ValidationUtils.addConstraintViolationKeepDefault(context,
                            "Column default should be a valid decimal number",
                            "columnDefault");
                    yield false;
                }

                yield true;
            }
            case "FLOAT", "REAL", "DOUBLE" -> {
                if (ValidationUtils.validateFloatValue(newDefaultValue)) {
                    yield true;
                }
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Column default should be a valid floating point number",
                        "columnDefault");
                yield false;
            }
            case "BOOLEAN" -> {
                if (ValidationUtils.validateBooleanValue(newDefaultValue)) {
                    yield true;
                }
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Column default should be a valid boolean (0, 1)",
                        "columnDefault");
                yield false;
            }
            case "DATE" -> {
                if (ValidationUtils.validateDateValue(newDefaultValue)) {
                    yield true;
                }
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Column default should be a valid date (yyyy-MM-dd)",
                        "columnDefault");
                yield false;
            }
            case "TIME" -> {
                if (ValidationUtils.validateTimeValue(newDefaultValue)) {
                    yield true;
                }
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Column default should be a valid time (HH:mm:ss)",
                        "columnDefault");
                yield false;
            }
            case "TIMESTAMP" -> {
                if (ValidationUtils.validateTimestampValue(newDefaultValue)) {
                    yield true;
                }
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Column default should be a valid timestamp (yyyy-MM-dd HH:mm:ss)",
                        "columnDefault");
                yield false;
            }
            default -> true;
        };
    }
}
