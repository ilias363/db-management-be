package ma.ilias.dbmanagementbe.record.utils;

import ma.ilias.dbmanagementbe.exception.InvalidFilterException;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.record.dto.FilterCriteriaDto;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;

import java.util.List;
import java.util.Set;

/**
 * Utility class for validating filter criteria against column metadata.
 * Ensures that operators and values are compatible with the column's data type.
 */
public class FilterValidationUtils {

    private static final Set<String> NUMERIC_TYPES = Set.of(
            "INT", "INTEGER", "SMALLINT", "BIGINT",
            "DECIMAL", "NUMERIC", "FLOAT", "REAL", "DOUBLE"
    );

    private static final Set<String> STRING_TYPES = Set.of(
            "VARCHAR", "CHAR", "TEXT"
    );

    private static final Set<String> DATE_TIME_TYPES = Set.of(
            "DATE", "TIME", "TIMESTAMP"
    );

    public static void validateFilter(FilterCriteriaDto filter, BaseColumnMetadataDto columnMeta) {
        if (filter == null || columnMeta == null) {
            return;
        }

        String dataType = columnMeta.getDataType().toUpperCase();
        FilterCriteriaDto.FilterOperator operator = filter.getOperator();

        // Validate operator compatibility with data type
        validateOperatorCompatibility(operator, dataType);

        // Validate values based on operator
        switch (operator) {
            case IS_NULL:
            case IS_NOT_NULL:
                // No value validation needed
                break;

            case EQUALS:
            case NOT_EQUALS:
            case GREATER_THAN:
            case GREATER_THAN_OR_EQUAL:
            case LESS_THAN:
            case LESS_THAN_OR_EQUAL:
                validateSingleValue(filter.getValue(), dataType, columnMeta);
                break;

            case BETWEEN:
                validateSingleValue(filter.getMinValue(), dataType, columnMeta);
                validateSingleValue(filter.getMaxValue(), dataType, columnMeta);
                if (filter.getMinValue() == null || filter.getMaxValue() == null) {
                    throw new InvalidFilterException("BETWEEN operator requires both minValue and maxValue");
                }
                break;

            case LIKE:
            case NOT_LIKE:
            case STARTS_WITH:
            case ENDS_WITH:
            case CONTAINS:
                validateStringValue(filter.getValue(), dataType);
                break;

            case IN:
            case NOT_IN:
                validateListValues(filter.getValues(), dataType, columnMeta);
                break;
        }
    }

    private static void validateOperatorCompatibility(FilterCriteriaDto.FilterOperator operator, String dataType) {
        switch (operator) {
            case IS_NULL:
            case IS_NOT_NULL:
            case EQUALS:
            case NOT_EQUALS:
            case IN:
            case NOT_IN:
                // These operators work with all data types
                break;

            case GREATER_THAN:
            case GREATER_THAN_OR_EQUAL:
            case LESS_THAN:
            case LESS_THAN_OR_EQUAL:
            case BETWEEN:
                if (!NUMERIC_TYPES.contains(dataType) && !DATE_TIME_TYPES.contains(dataType)) {
                    throw new InvalidFilterException(
                            String.format("Comparison operator %s is not compatible with data type %s. " +
                                            "Comparison operators can only be used with numeric or date/time types.",
                                    operator, dataType));
                }
                break;

            case LIKE:
            case NOT_LIKE:
            case STARTS_WITH:
            case ENDS_WITH:
            case CONTAINS:
                if (!STRING_TYPES.contains(dataType)) {
                    throw new InvalidFilterException(
                            String.format("String operator %s is not compatible with data type %s. " +
                                            "String operators can only be used with VARCHAR, CHAR, or TEXT types.",
                                    operator, dataType));
                }
                break;
        }
    }

    private static void validateSingleValue(Object value, String dataType, BaseColumnMetadataDto columnMeta) {
        if (value == null) {
            return; // Null values are generally allowed
        }

        String stringValue = value.toString();

        switch (dataType) {
            case "INT":
            case "INTEGER":
            case "SMALLINT":
            case "BIGINT":
                if (!ValidationUtils.validateIntegerValue(stringValue)) {
                    throw new InvalidFilterException(
                            String.format("Value '%s' is not a valid integer for column type %s",
                                    value, dataType));
                }
                break;

            case "DECIMAL":
            case "NUMERIC":
                if (!ValidationUtils.validateDecimalValue(stringValue)) {
                    throw new InvalidFilterException(
                            String.format("Value '%s' is not a valid decimal for column type %s",
                                    value, dataType));
                }
                // Additional validation for precision and scale
                if (columnMeta.getNumericPrecision() != null && columnMeta.getNumericScale() != null) {
                    if (!ValidationUtils.validateDecimalFormat(stringValue,
                            columnMeta.getNumericPrecision(), columnMeta.getNumericScale())) {
                        throw new InvalidFilterException(
                                String.format("Value '%s' exceeds the precision (%d) or scale (%d) for column type %s",
                                        value, columnMeta.getNumericPrecision(),
                                        columnMeta.getNumericScale(), dataType));
                    }
                }
                break;

            case "FLOAT":
            case "REAL":
            case "DOUBLE":
                if (!ValidationUtils.validateFloatValue(stringValue)) {
                    throw new InvalidFilterException(
                            String.format("Value '%s' is not a valid floating point number for column type %s",
                                    value, dataType));
                }
                break;

            case "BOOLEAN":
                if (!ValidationUtils.validateBooleanValue(stringValue)) {
                    throw new InvalidFilterException(
                            String.format("Value '%s' is not a valid boolean (expected 0 or 1) for column type %s",
                                    value, dataType));
                }
                break;

            case "DATE":
                if (!ValidationUtils.validateDateValue(stringValue)) {
                    throw new InvalidFilterException(
                            String.format("Value '%s' is not a valid date (expected yyyy-MM-dd format) for column type %s",
                                    value, dataType));
                }
                break;

            case "TIME":
                if (!ValidationUtils.validateTimeValue(stringValue)) {
                    throw new InvalidFilterException(
                            String.format("Value '%s' is not a valid time (expected HH:mm:ss format) for column type %s",
                                    value, dataType));
                }
                break;

            case "TIMESTAMP":
                if (!ValidationUtils.validateTimestampValue(stringValue)) {
                    throw new InvalidFilterException(
                            String.format("Value '%s' is not a valid timestamp for column type %s",
                                    value, dataType));
                }
                break;

            case "VARCHAR":
            case "CHAR":
                if (columnMeta.getCharacterMaxLength() != null &&
                        stringValue.length() > columnMeta.getCharacterMaxLength()) {
                    throw new InvalidFilterException(
                            String.format("Value '%s' exceeds maximum length (%d) for column type %s",
                                    value, columnMeta.getCharacterMaxLength(), dataType));
                }
                break;

            case "TEXT":
                // TEXT fields generally don't have length restrictions in the same way
                break;

            default:
                break;
        }
    }

    private static void validateStringValue(Object value, String dataType) {
        if (value == null) {
            throw new InvalidFilterException("String operators require a non-null value");
        }

        if (!STRING_TYPES.contains(dataType)) {
            throw new InvalidFilterException(
                    String.format("String value validation failed: column type %s does not support string operations",
                            dataType));
        }
    }

    private static void validateListValues(List<Object> values, String dataType, BaseColumnMetadataDto columnMeta) {
        if (values == null || values.isEmpty()) {
            throw new InvalidFilterException("IN/NOT_IN operators require a non-empty list of values");
        }

        for (Object value : values) {
            validateSingleValue(value, dataType, columnMeta);
        }
    }
}
