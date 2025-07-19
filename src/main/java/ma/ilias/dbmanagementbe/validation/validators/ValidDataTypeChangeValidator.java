package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnDataTypeDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.annotations.ValidDataTypeChange;

import java.util.Set;

@AllArgsConstructor
public class ValidDataTypeChangeValidator implements ConstraintValidator<ValidDataTypeChange, UpdateColumnDataTypeDto> {

    private ColumnService columnService;

    @Override
    public boolean isValid(UpdateColumnDataTypeDto value, ConstraintValidatorContext context) {
        if (value == null || value.getDataType() == null) {
            return true;
        }

        try {
            BaseColumnMetadataDto currentColumn = columnService.getColumn(
                    value.getSchemaName(),
                    value.getTableName(),
                    value.getColumnName()
            );

            if (currentColumn == null) {
                return true;
            }

            String currentDataType = currentColumn.getDataType().toUpperCase();
            String newDataType = value.getDataType().toUpperCase();

            // if data type is the same, let manager handle the length, precision, and scale change
            if (currentDataType.equals(newDataType)) {
                return true;
            }

            return isDataTypeChangeCompatible(currentDataType, newDataType, context);

        } catch (Exception e) {
            return true;
        }
    }

    private boolean isDataTypeChangeCompatible(String currentDataType, String newDataType, ConstraintValidatorContext context) {
        // Generally safe conversions (widening conversions)
        if (isSafeWidening(currentDataType, newDataType)) {
            return true;
        }

        // String type conversions
        if (isStringTypeConversion(newDataType)) {
            return true; // Let manager handle actual data validation
        }

        // Numeric type conversions
        if (isNumericTypeConversion(currentDataType, newDataType)) {
            return true; // Let manager handle precision/scale validation
        }

        // Date/Time conversions
        if (isDateTimeConversion(currentDataType, newDataType)) {
            return true; // Let manager handle format validation
        }

        // Potentially unsafe conversions - let manager handle with proper error messages
        if (isPotentiallyUnsafeConversion(currentDataType, newDataType)) {
            return true; // Let manager validate actual data compatibility
        }

        // Completely incompatible types
        context.disableDefaultConstraintViolation();
        context.buildConstraintViolationWithTemplate(
                        String.format("Data type conversion from %s to %s is not supported", currentDataType, newDataType))
                .addPropertyNode("dataType")
                .addConstraintViolation();
        return false;
    }

    private boolean isSafeWidening(String currentDataType, String newDataType) {
        return switch (currentDataType.toUpperCase()) {
            case "SMALLINT" ->
                    Set.of("INT", "INTEGER", "BIGINT", "DECIMAL", "NUMERIC", "FLOAT", "DOUBLE", "REAL").contains(newDataType.toUpperCase());
            case "INT", "INTEGER" ->
                    Set.of("BIGINT", "DECIMAL", "NUMERIC", "FLOAT", "DOUBLE", "REAL").contains(newDataType.toUpperCase());
            case "BIGINT" ->
                    Set.of("DECIMAL", "NUMERIC", "FLOAT", "DOUBLE", "REAL").contains(newDataType.toUpperCase());
            case "FLOAT", "REAL" -> Set.of("DOUBLE", "DECIMAL", "NUMERIC").contains(newDataType.toUpperCase());
            case "CHAR", "VARCHAR" -> "TEXT".equalsIgnoreCase(newDataType);
            case "DATE" -> "TIMESTAMP".equalsIgnoreCase(newDataType);
            default -> false;
        };
    }

    private boolean isStringTypeConversion(String newDataType) {
        // Any type can be converted to string types (with potential data loss)
        return "VARCHAR".equals(newDataType) || "CHAR".equals(newDataType) || "TEXT".equals(newDataType);
    }

    private boolean isNumericTypeConversion(String currentDataType, String newDataType) {
        // Numeric to numeric conversions
        return isNumericType(currentDataType) && isNumericType(newDataType);
    }

    private boolean isDateTimeConversion(String currentDataType, String newDataType) {
        // Date/Time to Date/Time conversions
        return isDateTimeType(currentDataType) && isDateTimeType(newDataType);
    }

    private boolean isPotentiallyUnsafeConversion(String currentDataType, String newDataType) {
        // String to numeric (if all values are numeric)
        if (isStringType(currentDataType) && isNumericType(newDataType)) {
            return true;
        }

        // String to date/time (if all values are in correct format)
        if (isStringType(currentDataType) && isDateTimeType(newDataType)) {
            return true;
        }

        // Boolean conversions
        return "BOOLEAN".equals(currentDataType) || "BOOLEAN".equals(newDataType);
    }

    private boolean isNumericType(String dataType) {
        return switch (dataType) {
            case "SMALLINT", "INT", "INTEGER", "BIGINT", "DECIMAL", "NUMERIC",
                 "FLOAT", "REAL", "DOUBLE" -> true;
            default -> false;
        };
    }

    private boolean isStringType(String dataType) {
        return "VARCHAR".equals(dataType) || "CHAR".equals(dataType) || "TEXT".equals(dataType);
    }

    private boolean isDateTimeType(String dataType) {
        return "DATE".equals(dataType) || "TIME".equals(dataType) || "TIMESTAMP".equals(dataType);
    }
}
