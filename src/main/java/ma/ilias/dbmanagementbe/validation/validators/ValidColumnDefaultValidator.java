package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.standard.NewStandardColumnDto;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ValidColumnDefault;

public class ValidColumnDefaultValidator implements ConstraintValidator<ValidColumnDefault, BaseNewColumnDto> {

    @Override
    public boolean isValid(BaseNewColumnDto dto, ConstraintValidatorContext context) {
        if (dto == null) {
            return true;
        }

        String dataType = null;
        String defaultValue = null;
        Boolean isNullable = null;
        Boolean isUnique = null;
        Integer charLen = null;
        Integer numPrecision = null;
        Integer numScale = null;

        if (dto instanceof NewStandardColumnDto standardCol) {
            dataType = standardCol.getDataType();
            defaultValue = standardCol.getColumnDefault();
            isNullable = standardCol.getIsNullable();
            isUnique = standardCol.getIsUnique();
            charLen = standardCol.getCharacterMaxLength();
            numPrecision = standardCol.getNumericPrecision();
            numScale = standardCol.getNumericScale();
        } else if (dto instanceof NewForeignKeyColumnDto fkCol) {
            dataType = fkCol.getDataType();
            defaultValue = fkCol.getColumnDefault();
            isNullable = fkCol.getIsNullable();
            charLen = fkCol.getCharacterMaxLength();
            numPrecision = fkCol.getNumericPrecision();
            numScale = fkCol.getNumericScale();
        }

        if (defaultValue == null || defaultValue.isEmpty())
            return true;

        if (!defaultValue.equalsIgnoreCase("NULL") && isUnique != null && isUnique) {
            ValidationUtils.addConstraintViolation(context,
                    "If a column is unique, the default value should be null",
                    "columnDefault");
            return false;
        }

        if (isNullable != null && isNullable && "NULL".equalsIgnoreCase(defaultValue))
            return true;
        if (dataType == null)
            return true;

        context.disableDefaultConstraintViolation();

        return switch (dataType.toUpperCase()) {
            case "VARCHAR", "CHAR" -> {
                if (charLen == null || defaultValue.length() <= charLen) {
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
                if (ValidationUtils.validateIntegerValue(defaultValue))
                    yield true;
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Column default should be a valid integer",
                        "columnDefault");
                yield false;
            }
            case "DECIMAL", "NUMERIC" -> {
                if (numPrecision != null && numScale != null) {
                    if (!ValidationUtils.validateDecimalFormat(defaultValue, numPrecision, numScale)) {
                        ValidationUtils.addConstraintViolationKeepDefault(context,
                                "Decimal column default should have a valid format (precision, scale)",
                                "columnDefault");
                        yield false;
                    }
                }

                if (!ValidationUtils.validateDecimalValue(defaultValue)) {
                    ValidationUtils.addConstraintViolationKeepDefault(context,
                            "Column default should be a valid decimal number",
                            "columnDefault");
                    yield false;
                }

                yield true;
            }
            case "FLOAT", "REAL", "DOUBLE" -> {
                if (ValidationUtils.validateFloatValue(defaultValue)) {
                    yield true;
                }
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Column default should be a valid floating point number",
                        "columnDefault");
                yield false;
            }
            case "BOOLEAN" -> {
                if (ValidationUtils.validateBooleanValue(defaultValue)) {
                    yield true;
                }
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Column default should be a valid boolean (0, 1)",
                        "columnDefault");
                yield false;
            }
            case "DATE" -> {
                if (ValidationUtils.validateDateValue(defaultValue)) {
                    yield true;
                }
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Column default should be a valid date (yyyy-MM-dd)",
                        "columnDefault");
                yield false;
            }
            case "TIME" -> {
                if (ValidationUtils.validateTimeValue(defaultValue)) {
                    yield true;
                }
                ValidationUtils.addConstraintViolationKeepDefault(context,
                        "Column default should be a valid time (HH:mm:ss)",
                        "columnDefault");
                yield false;
            }
            case "TIMESTAMP" -> {
                if (ValidationUtils.validateTimestampValue(defaultValue)) {
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
