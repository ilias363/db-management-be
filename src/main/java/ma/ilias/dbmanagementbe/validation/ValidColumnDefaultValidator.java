package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;

import java.util.regex.Pattern;

public class ValidColumnDefaultValidator implements ConstraintValidator<ValidColumnDefault, BaseNewColumnDto> {

    @Override
    public boolean isValid(BaseNewColumnDto dto, ConstraintValidatorContext context) {
        String dataType = dto.getDataType();
        String defaultValue = dto.getColumnDefault();
        Boolean isNullable = dto.getIsNullable();
        Boolean isUnique = dto.getIsUnique();
        Integer charLen = dto.getCharacterMaxLength();
        Integer numPrecision = dto.getNumericPrecision();
        Integer numScale = dto.getNumericScale();
        Boolean autoIncrement = dto.getAutoIncrement();

        if (defaultValue == null || defaultValue.isEmpty()) return true;
        if (!defaultValue.equalsIgnoreCase("NULL") && isUnique != null && isUnique) {
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate("If a column is unique, the default value should be null")
                    .addPropertyNode("columnDefault")
                    .addConstraintViolation();
            return false;
        }
        if (isNullable != null && isNullable && "NULL".equalsIgnoreCase(defaultValue)) return true;
        if (dataType == null) return true;

        context.disableDefaultConstraintViolation();

        switch (dataType.toUpperCase()) {
            case "VARCHAR":
            case "CHAR":
                if (charLen == null || defaultValue.length() <= charLen) return true;
                context.buildConstraintViolationWithTemplate("Column default string length is bigger than the maximum allowed length")
                        .addPropertyNode("columnDefault").addConstraintViolation();
                return false;
            case "TEXT":
                context.buildConstraintViolationWithTemplate("Data type TEXT cannot have default")
                        .addPropertyNode("columnDefault").addConstraintViolation();
                return false;
            case "INT":
            case "INTEGER":
            case "SMALLINT":
            case "BIGINT":
                if (autoIncrement != null && autoIncrement) return true;
                try {
                    Integer.parseInt(defaultValue);
                    return true;
                } catch (NumberFormatException e) {
                    context.buildConstraintViolationWithTemplate("Column default should be a valid integer")
                            .addPropertyNode("columnDefault").addConstraintViolation();
                    return false;
                }
            case "DECIMAL":
            case "NUMERIC":
                try {
                    if (numPrecision != null && numScale != null) {
                        String regex = "^-?\\d{1," + (numPrecision - numScale) + "}(\\.\\d{1," + numScale + "})?$";
                        if (!Pattern.matches(regex, defaultValue)) {
                            context.buildConstraintViolationWithTemplate("Column default should be a valid decimal number")
                                    .addPropertyNode("columnDefault").addConstraintViolation();
                            return false;
                        }
                    }
                    Double.parseDouble(defaultValue);
                    return true;
                } catch (NumberFormatException e) {
                    context.buildConstraintViolationWithTemplate("Column default should be a valid decimal number")
                            .addPropertyNode("columnDefault").addConstraintViolation();
                    return false;
                }
            case "FLOAT":
            case "REAL":
            case "DOUBLE":
                try {
                    Double.parseDouble(defaultValue);
                    return true;
                } catch (NumberFormatException e) {
                    context.buildConstraintViolationWithTemplate("Column default should be a valid number")
                            .addPropertyNode("columnDefault").addConstraintViolation();
                    return false;
                }
            case "BOOLEAN":
                if ("0".equals(defaultValue) || "1".equals(defaultValue)) return true;
                context.buildConstraintViolationWithTemplate("Column default should be a valid boolean (0, 1)")
                        .addPropertyNode("columnDefault").addConstraintViolation();
                return false;
            case "DATE":
                if (Pattern.matches("^\\d{4}-\\d{2}-\\d{2}$", defaultValue)) return true;
                context.buildConstraintViolationWithTemplate("Column default should be a valid date (yyyy-MM-dd)")
                        .addPropertyNode("columnDefault").addConstraintViolation();
                return false;
            case "TIME":
                if (Pattern.matches("^\\d{2}:\\d{2}:\\d{2}$", defaultValue)) return true;
                context.buildConstraintViolationWithTemplate("Column default should be a valid time (HH:mm:ss)")
                        .addPropertyNode("columnDefault").addConstraintViolation();
                return false;
            case "TIMESTAMP":
                if (
                        "CURRENT_TIMESTAMP".equalsIgnoreCase(defaultValue) ||
                                Pattern.matches("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", defaultValue)
                ) return true;
                context.buildConstraintViolationWithTemplate("Column default should be ''CURRENT_TIMESTAMP'' or a valid timestamp (yyyy-MM-dd HH:mm:ss)")
                        .addPropertyNode("columnDefault").addConstraintViolation();
                return false;
            default:
                return true;
        }
    }
}
