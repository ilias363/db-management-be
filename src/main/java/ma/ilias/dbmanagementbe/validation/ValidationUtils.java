package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.common.ColumnDataTypeDefinition;
import org.springframework.jdbc.core.JdbcTemplate;

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.regex.Pattern;

/**
 * Utility class containing reusable validation logic extracted from validators.
 * This class provides common methods for handling validation constraints,
 * error messages, and data type validations.
 */
public class ValidationUtils {

    // Common data type sets for validation
    public static final Set<String> NUMERIC_DATA_TYPES = Set.of(
            "INT", "INTEGER", "SMALLINT", "BIGINT", "FLOAT", "REAL", "DOUBLE");

    public static final Set<String> CHARACTER_DATA_TYPES = Set.of(
            "VARCHAR", "CHAR");

    public static final Set<String> DECIMAL_DATA_TYPES = Set.of(
            "DECIMAL", "NUMERIC");

    public static final Set<String> AUTO_INCREMENT_COMPATIBLE_TYPES = Set.of(
            "INT", "INTEGER", "SMALLINT", "BIGINT", "FLOAT", "REAL", "DOUBLE");

    // Common validation patterns
    public static final Pattern DATE_PATTERN = Pattern.compile("^\\d{4}-\\d{2}-\\d{2}$");
    public static final Pattern TIME_PATTERN = Pattern.compile("^\\d{2}:\\d{2}:\\d{2}$");
    public static final Pattern TIMESTAMP_PATTERN = Pattern.compile("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$");

    /**
     * Safely checks if any of the provided strings are null or blank.
     *
     * @param values the strings to check
     * @return true if any string is null or blank
     */
    public static boolean hasNullOrBlankValues(String... values) {
        if (values == null || values.length == 0) {
            return true;
        }
        for (String value : values) {
            if (value == null || value.isBlank()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Disables the default constraint violation and builds a new one with the
     * specified message and property path.
     *
     * @param context      the validation context
     * @param message      the error message
     * @param propertyPath the property path for the error
     */
    public static void addConstraintViolation(ConstraintValidatorContext context, String message, String propertyPath) {
        context.disableDefaultConstraintViolation();
        context.buildConstraintViolationWithTemplate(message)
                .addPropertyNode(propertyPath)
                .addConstraintViolation();
    }

    /**
     * Adds a constraint violation without disabling the default violation.
     *
     * @param context      the validation context
     * @param message      the error message
     * @param propertyPath the property path for the error
     */
    public static void addConstraintViolationKeepDefault(ConstraintValidatorContext context, String message,
                                                         String propertyPath) {
        context.buildConstraintViolationWithTemplate(message)
                .addPropertyNode(propertyPath)
                .addConstraintViolation();
    }

    /**
     * Validates that character and numeric fields are null for data types that
     * don't support them.
     *
     * @param dto     the column data type definition
     * @param context the validation context
     * @return true if validation passes
     */
    public static boolean validateNumericAndCharFieldsNull(ColumnDataTypeDefinition dto,
                                                           ConstraintValidatorContext context) {
        return validateNumericFieldsNull(dto, context) && validateCharFieldNull(dto, context);
    }

    /**
     * Validates that numeric fields (precision and scale) are null.
     *
     * @param dto     the column data type definition
     * @param context the validation context
     * @return true if validation passes
     */
    public static boolean validateNumericFieldsNull(ColumnDataTypeDefinition dto, ConstraintValidatorContext context) {
        if (dto.getNumericPrecision() != null || dto.getNumericScale() != null) {
            addConstraintViolation(context,
                    "NumericPrecision and numericScale must be null for this data type",
                    "dataType");
            return false;
        }
        return true;
    }

    /**
     * Validates that character field (characterMaxLength) is null.
     *
     * @param dto     the column data type definition
     * @param context the validation context
     * @return true if validation passes
     */
    public static boolean validateCharFieldNull(ColumnDataTypeDefinition dto, ConstraintValidatorContext context) {
        if (dto.getCharacterMaxLength() != null) {
            addConstraintViolation(context,
                    "characterMaxLength must be null for this data type",
                    "dataType");
            return false;
        }
        return true;
    }

    /**
     * Validates VARCHAR/CHAR data type requirements.
     *
     * @param dto     the column data type definition
     * @param context the validation context
     * @return true if validation passes
     */
    public static boolean validateVarcharOrChar(ColumnDataTypeDefinition dto, ConstraintValidatorContext context) {
        context.disableDefaultConstraintViolation();

        if (dto.getCharacterMaxLength() == null) {
            addConstraintViolationKeepDefault(context,
                    "characterMaxLength is required for VARCHAR/CHAR types",
                    "characterMaxLength");
            return false;
        }

        if (dto.getCharacterMaxLength() <= 0) {
            addConstraintViolationKeepDefault(context,
                    "characterMaxLength must be positive",
                    "characterMaxLength");
            return false;
        }

        return validateNumericFieldsNull(dto, context);
    }

    /**
     * Validates DECIMAL/NUMERIC data type requirements.
     *
     * @param dto     the column data type definition
     * @param context the validation context
     * @return true if validation passes
     */
    public static boolean validateDecimalOrNumeric(ColumnDataTypeDefinition dto, ConstraintValidatorContext context) {
        context.disableDefaultConstraintViolation();
        boolean isValid = true;

        if (dto.getNumericPrecision() == null) {
            addConstraintViolationKeepDefault(context,
                    "numericPrecision is required for DECIMAL/NUMERIC types",
                    "numericPrecision");
            isValid = false;
        }

        if (dto.getNumericScale() == null) {
            addConstraintViolationKeepDefault(context,
                    "numericScale is required for DECIMAL/NUMERIC types",
                    "numericScale");
            isValid = false;
        }

        if (!isValid)
            return false; // Stop if fields are null

        if (dto.getNumericPrecision() <= 0) {
            addConstraintViolationKeepDefault(context,
                    "numericPrecision must be positive",
                    "numericPrecision");
            isValid = false;
        }

        if (dto.getNumericScale() < 0) {
            addConstraintViolationKeepDefault(context,
                    "numericScale cannot be negative",
                    "numericScale");
            isValid = false;
        }

        if (!isValid)
            return false; // Stop if values are invalid

        if (dto.getNumericPrecision() < dto.getNumericScale()) {
            addConstraintViolationKeepDefault(context,
                    "numericPrecision must be greater than or equal to numericScale",
                    "numericPrecision");
            isValid = false;
        }

        return isValid && validateCharFieldNull(dto, context);
    }

    /**
     * Validates if a data type is compatible with auto-increment.
     *
     * @param dataType the data type to check
     * @return true if the data type supports auto-increment
     */
    public static boolean isAutoIncrementCompatible(String dataType) {
        if (dataType == null)
            return false;
        return AUTO_INCREMENT_COMPATIBLE_TYPES.contains(dataType.toUpperCase());
    }

    /**
     * Validates an integer string value.
     */
    public static boolean validateIntegerValue(String value) {
        try {
            Integer.parseInt(value);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    /**
     * Validates a decimal string value.
     */
    public static boolean validateDecimalValue(String value) {
        try {
            Double.parseDouble(value);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    /**
     * Validates a float string value.
     */
    public static boolean validateFloatValue(String value) {
        try {
            Double.parseDouble(value);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    /**
     * Validates a boolean string value.
     */
    public static boolean validateBooleanValue(String value) {
        return "0".equals(value) || "1".equals(value);
    }

    /**
     * Validates a date string value.
     */
    public static boolean validateDateValue(String value) {
        return DATE_PATTERN.matcher(value).matches();
    }

    /**
     * Validates a time string value.
     */
    public static boolean validateTimeValue(String value) {
        return TIME_PATTERN.matcher(value).matches();
    }

    /**
     * Validates a timestamp string value.
     */
    public static boolean validateTimestampValue(String value) {
        return "CURRENT_TIMESTAMP".equalsIgnoreCase(value) ||
                TIMESTAMP_PATTERN.matcher(value).matches();
    }

    /**
     * Validates that a decimal value matches the precision and scale requirements.
     *
     * @param value     the decimal value as string
     * @param precision the numeric precision
     * @param scale     the numeric scale
     * @return true if the decimal format is valid
     */
    public static boolean validateDecimalFormat(String value, Integer precision, Integer scale) {
        if (value == null || precision == null || scale == null)
            return true;

        String regex = "^-?\\d{1," + (precision - scale) + "}(\\.\\d{1," + scale + "})?$";
        return Pattern.matches(regex, value);
    }

    /**
     * Validates existence of entities by their IDs using a repository check
     * function. if context or errorMessage is null, it will not add
     * constraint violations.
     *
     * @param ids           the collection of IDs to check
     * @param existsChecker function that checks if an ID exists
     * @param context       the validation context
     * @param errorMessage  the error message for non-existent entities
     * @param <T>           the type of the ID
     * @return true if all entities exist
     */
    public static <T> boolean validateEntitiesExist(
            Collection<T> ids,
            Predicate<T> existsChecker,
            ConstraintValidatorContext context,
            String errorMessage) {

        if (ids == null || ids.isEmpty()) {
            return true;
        }

        for (T id : ids) {
            if (id != null && !existsChecker.test(id)) {
                if (context != null && errorMessage != null) {
                    addConstraintViolation(context, errorMessage, null);
                }
                return false;
            }
        }
        return true;

    }

    /**
     * Extracts ID from an object that can be either a Long or a DTO with getId()
     * method.
     *
     * @param item the item to extract ID from
     * @return the extracted ID or null if extraction fails
     */
    public static Long extractId(Object item) {
        if (item instanceof Long) {
            return (Long) item;
        }
        try {
            // Use reflection to call getId() method
            Method getIdMethod = item.getClass().getMethod("getId");
            Object result = getIdMethod.invoke(item);
            return result instanceof Long ? (Long) result : null;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Validates uniqueness of a field value, handling both create and update
     * scenarios.
     *
     * @param repository     the repository instance with findByFieldName method
     * @param fieldValue     the value to check for uniqueness
     * @param entityId       the ID of the entity (null for create, existing ID for
     *                       update)
     * @param findMethodName the name of the find method (e.g., "findByName",
     *                       "findByEmail")
     * @return true if the value is unique or belongs to the same entity being
     * updated
     */
    public static boolean validateUniqueness(
            Object repository,
            String fieldValue,
            Long entityId,
            String findMethodName) {

        if (fieldValue == null) {
            return true;
        }

        try {
            Method findMethod = repository.getClass().getMethod(findMethodName, String.class);
            Object result = findMethod.invoke(repository, fieldValue);

            if (result instanceof Optional<?> optional) {
                if (optional.isEmpty()) {
                    return true;
                }

                // For create operations (entityId is null), any existing entity makes it
                // non-unique
                if (entityId == null) {
                    return false;
                }

                // For update operations, check if the found entity is the same as the one being
                // updated
                Object foundEntity = optional.get();
                Method getIdMethod = foundEntity.getClass().getMethod("getId");
                Long foundId = (Long) getIdMethod.invoke(foundEntity);
                return foundId.equals(entityId);
            }

            return true; // If not Optional, assume unique (fallback)
        } catch (Exception e) {
            // If reflection fails, assume validation passes to avoid breaking the
            // application
            return true;
        }
    }

    /**
     * Validates that two data types match for foreign key relationships.
     *
     * @param dataType1    the first data type
     * @param dataType2    the second data type
     * @param context      the validation context
     * @param propertyPath the property path for the error
     * @return true if data types match
     */
    public static boolean validateDataTypeMatch(
            String dataType1, String dataType2,
            ConstraintValidatorContext context, String propertyPath) {
        if (dataType1 == null || dataType2 == null) {
            return true;
        }

        if (!dataType1.equalsIgnoreCase(dataType2)) {
            addConstraintViolationKeepDefault(context,
                    String.format("Data type '%s' does not match referenced column's data type '%s'",
                            dataType1, dataType2),
                    propertyPath);
            return false;
        }
        return true;
    }

    /**
     * Validates that character properties match between two columns.
     *
     * @param charLength1 the first character length
     * @param charLength2 the second character length
     * @param context     the validation context
     * @return true if character properties match
     */
    public static boolean validateCharacterPropertiesMatch(
            Long charLength1, Long charLength2,
            ConstraintValidatorContext context) {

        if (!isLongEqual(charLength1, charLength2)) {
            addConstraintViolationKeepDefault(context,
                    String.format(
                            "Character max length '%s' does not match referenced column's character max length '%s'",
                            charLength1, charLength2),
                    "characterMaxLength");
            return false;
        }
        return true;
    }

    /**
     * Validates that numeric properties match between two columns.
     *
     * @param precision1 the first numeric precision
     * @param precision2 the second numeric precision
     * @param scale1     the first numeric scale
     * @param scale2     the second numeric scale
     * @param context    the validation context
     * @return true if numeric properties match
     */
    public static boolean validateNumericPropertiesMatch(
            Integer precision1, Integer precision2,
            Integer scale1, Integer scale2,
            ConstraintValidatorContext context) {
        boolean isValid = true;

        if (!isIntegerEqual(precision1, precision2)) {
            addConstraintViolationKeepDefault(context,
                    String.format("Numeric precision '%s' does not match referenced column's numeric precision '%s'",
                            precision1, precision2),
                    "numericPrecision");
            isValid = false;
        }

        if (!isIntegerEqual(scale1, scale2)) {
            addConstraintViolationKeepDefault(context,
                    String.format("Numeric scale '%s' does not match referenced column's numeric scale '%s'", scale1,
                            scale2),
                    "numericScale");
            isValid = false;
        }

        return isValid;
    }

    /**
     * Checks if two Integer values are equal, considering null values.
     *
     * @param value1 the first value
     * @param value2 the second value
     * @return true if both values are equal (including both being null)
     */
    public static boolean isIntegerEqual(Integer value1, Integer value2) {
        if (value1 == null && value2 == null)
            return true;
        if (value1 == null || value2 == null)
            return false;
        return value1.equals(value2);
    }

    /**
     * Checks if two Long values are equal, considering null values.
     *
     * @param value1 the first value
     * @param value2 the second value
     * @return true if both values are equal (including both being null)
     */
    public static boolean isLongEqual(Long value1, Long value2) {
        if (value1 == null && value2 == null)
            return true;
        if (value1 == null || value2 == null)
            return false;
        return value1.equals(value2);
    }

    /**
     * Validates if a string value corresponds to a valid enum constant.
     *
     * @param value     the string value to validate
     * @param enumClass the enum class to check against
     * @param <E>       the enum type
     * @return true if the value matches an enum constant
     */
    public static <E extends Enum<E>> boolean validateEnum(String value, Class<E> enumClass) {
        if (value == null || enumClass == null) {
            return true;
        }

        try {
            Enum.valueOf(enumClass, value.toUpperCase());
            return true;
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    /**
     * Validates if a data type is numeric.
     *
     * @param dataType the data type to check
     * @return true if the data type is numeric
     */
    public static boolean isNumericDataType(String dataType) {
        if (dataType == null)
            return false;
        return NUMERIC_DATA_TYPES.contains(dataType.toUpperCase()) ||
                DECIMAL_DATA_TYPES.contains(dataType.toUpperCase());
    }

    /**
     * Validates if a data type is a string type.
     *
     * @param dataType the data type to check
     * @return true if the data type is a string type
     */
    public static boolean isStringDataType(String dataType) {
        if (dataType == null)
            return false;
        return CHARACTER_DATA_TYPES.contains(dataType.toUpperCase()) || "TEXT".equalsIgnoreCase(dataType);
    }

    /**
     * Validates if a data type is a date/time type.
     *
     * @param dataType the data type to check
     * @return true if the data type is a date/time type
     */
    public static boolean isDateTimeDataType(String dataType) {
        if (dataType == null)
            return false;
        String upperType = dataType.toUpperCase();
        return "DATE".equals(upperType) || "TIME".equals(upperType) || "TIMESTAMP".equals(upperType);
    }

    /**
     * Validates that a string field is required and not blank when a condition is
     * met.
     *
     * @param fieldValue   the string field value
     * @param context      the validation context
     * @param errorMessage the error message to display
     * @param propertyPath the property path for the error
     * @return true if validation passes
     */
    public static boolean validateRequiredStringField(String fieldValue, ConstraintValidatorContext context,
                                                      String errorMessage, String propertyPath) {
        if (fieldValue == null || fieldValue.isBlank()) {
            addConstraintViolationKeepDefault(context, errorMessage, propertyPath);
            return false;
        }
        return true;
    }

    /**
     * Validates existence of a value in a referenced table using JDBC.
     *
     * @param jdbcTemplate the JDBC template to execute queries
     * @param schemaName   the schema name
     * @param tableName    the table name
     * @param columnName   the column name
     * @param value        the value to check
     * @param context      the validation context
     * @param propertyPath the property path for the error
     * @return true if the value exists in the referenced table
     */
    public static boolean validateValueExistsInReferencedTable(
            JdbcTemplate jdbcTemplate, String schemaName, String tableName,
            String columnName, String value, ConstraintValidatorContext context,
            String propertyPath) {

        try {
            String checkSql = String.format(
                    "SELECT COUNT(*) FROM %s.%s WHERE %s = ?",
                    schemaName, tableName, columnName);

            Integer count = jdbcTemplate.queryForObject(checkSql, Integer.class, value);

            if (count == null || count == 0) {
                addConstraintViolation(context,
                        String.format("Default value '%s' does not exist in the referenced table '%s.%s' column '%s'",
                                value, schemaName, tableName, columnName),
                        propertyPath);
                return false;
            }
            return true;
        } catch (Exception e) {
            addConstraintViolation(context, "Unable to validate default value in referenced table", propertyPath);
            return false;
        }
    }

    /**
     * Validates data type compatibility for data type changes.
     *
     * @param currentDataType the current data type
     * @param newDataType     the new data type to change to
     * @param context         the validation context
     * @return true if the data type change is compatible
     */
    public static boolean validateDataTypeChangeCompatibility(String currentDataType, String newDataType,
                                                              ConstraintValidatorContext context) {
        if (currentDataType == null || newDataType == null) {
            return true;
        }

        String upperCurrentType = currentDataType.toUpperCase();
        String upperNewType = newDataType.toUpperCase();

        // If data type is the same, let manager handle the length, precision, and scale
        // change
        if (upperCurrentType.equals(upperNewType)) {
            return true;
        }

        // Generally safe conversions (widening conversions)
        if (isSafeWideningConversion(upperCurrentType, upperNewType)) {
            return true;
        }

        // String type conversions
        if (isStringDataType(upperNewType)) {
            return true; // Let manager handle actual data validation
        }

        // Numeric type conversions
        if (isNumericDataType(upperCurrentType) && isNumericDataType(upperNewType)) {
            return true; // Let manager handle precision/scale validation
        }

        // Date/Time conversions
        if (isDateTimeDataType(upperCurrentType) && isDateTimeDataType(upperNewType)) {
            return true; // Let manager handle format validation
        }

        // Potentially unsafe conversions - let manager handle with proper error
        // messages
        if (isPotentiallyUnsafeConversion(upperCurrentType, upperNewType)) {
            return true; // Let manager validate actual data compatibility
        }

        // Completely incompatible types
        addConstraintViolation(context,
                String.format("Data type conversion from %s to %s is not supported", upperCurrentType, upperNewType),
                "dataType");
        return false;
    }

    /**
     * Checks if a data type conversion is a safe widening conversion.
     *
     * @param currentDataType the current data type
     * @param newDataType     the new data type
     * @return true if the conversion is safe widening
     */
    public static boolean isSafeWideningConversion(String currentDataType, String newDataType) {
        return switch (currentDataType.toUpperCase()) {
            case "SMALLINT" -> Set.of("INT", "INTEGER", "BIGINT", "DECIMAL", "NUMERIC", "FLOAT", "DOUBLE", "REAL")
                    .contains(newDataType.toUpperCase());
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

    /**
     * Checks if a data type conversion is potentially unsafe but allowed.
     *
     * @param currentDataType the current data type
     * @param newDataType     the new data type
     * @return true if the conversion is potentially unsafe but allowed
     */
    public static boolean isPotentiallyUnsafeConversion(String currentDataType, String newDataType) {
        // String to numeric (if all values are numeric)
        if (isStringDataType(currentDataType) && isNumericDataType(newDataType)) {
            return true;
        }

        // String to date/time (if all values are in correct format)
        if (isStringDataType(currentDataType) && isDateTimeDataType(newDataType)) {
            return true;
        }

        // Boolean conversions
        return "BOOLEAN".equals(currentDataType) || "BOOLEAN".equals(newDataType);
    }
}