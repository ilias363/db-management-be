package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.common.ColumnDataTypeDefinition;
import ma.ilias.dbmanagementbe.validation.annotations.ValidDataTypeDefinition;

public class ValidDataTypeDefinitionValidator implements ConstraintValidator<ValidDataTypeDefinition, ColumnDataTypeDefinition> {

    @Override
    public boolean isValid(ColumnDataTypeDefinition dto, ConstraintValidatorContext context) {
        if (dto == null || dto.getDataType() == null) {
            return true; // Handled by @NotBlank on the field itself
        }

        String dataType = dto.getDataType().toUpperCase();

        return switch (dataType) {
            case "VARCHAR", "CHAR" -> isVarcharOrCharValid(dto, context);
            case "DECIMAL", "NUMERIC" -> isDecimalOrNumericValid(dto, context);
            case "INT", "INTEGER", "SMALLINT", "BIGINT", "BOOLEAN", "DATE", "TIME", "TIMESTAMP", "TEXT", "FLOAT",
                 "REAL", "DOUBLE" -> areNumericAndCharFieldsNull(dto, context);
            default -> true; // The @Pattern annotation will handle unknown types
        };
    }

    private boolean isVarcharOrCharValid(ColumnDataTypeDefinition dto, ConstraintValidatorContext context) {
        context.disableDefaultConstraintViolation();
        if (dto.getCharacterMaxLength() == null) {
            context.buildConstraintViolationWithTemplate("characterMaxLength is required for VARCHAR/CHAR types")
                    .addPropertyNode("characterMaxLength").addConstraintViolation();
            return false;
        }
        if (dto.getCharacterMaxLength() <= 0) {
            context.buildConstraintViolationWithTemplate("characterMaxLength must be positive")
                    .addPropertyNode("characterMaxLength").addConstraintViolation();
            return false;
        }
        return areNumericFieldsNull(dto, context);
    }

    private boolean isDecimalOrNumericValid(ColumnDataTypeDefinition dto, ConstraintValidatorContext context) {
        context.disableDefaultConstraintViolation();
        boolean isValid = true;
        if (dto.getNumericPrecision() == null) {
            context.buildConstraintViolationWithTemplate("numericPrecision is required for DECIMAL/NUMERIC types")
                    .addPropertyNode("numericPrecision").addConstraintViolation();
            isValid = false;
        }
        if (dto.getNumericScale() == null) {
            context.buildConstraintViolationWithTemplate("numericScale is required for DECIMAL/NUMERIC types")
                    .addPropertyNode("numericScale").addConstraintViolation();
            isValid = false;
        }

        if (!isValid) return false; // Stop if fields are null

        if (dto.getNumericPrecision() <= 0) {
            context.buildConstraintViolationWithTemplate("numericPrecision must be positive")
                    .addPropertyNode("numericPrecision").addConstraintViolation();
            isValid = false;
        }
        if (dto.getNumericScale() < 0) {
            context.buildConstraintViolationWithTemplate("numericScale cannot be negative")
                    .addPropertyNode("numericScale").addConstraintViolation();
            isValid = false;
        }

        if (!isValid) return false; // Stop if values are invalid

        if (dto.getNumericPrecision() < dto.getNumericScale()) {
            context.buildConstraintViolationWithTemplate("numericPrecision must be greater than or equal to numericScale")
                    .addPropertyNode("numericPrecision").addConstraintViolation();
            isValid = false;
        }

        return isValid && isCharFieldNull(dto, context);
    }

    private boolean areNumericFieldsNull(ColumnDataTypeDefinition dto, ConstraintValidatorContext context) {
        if (dto.getNumericPrecision() != null || dto.getNumericScale() != null) {
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate("NumericPrecision and numericScale must be null for this data type")
                    .addPropertyNode("dataType").addConstraintViolation();
            return false;
        }
        return true;
    }

    private boolean isCharFieldNull(ColumnDataTypeDefinition dto, ConstraintValidatorContext context) {
        if (dto.getCharacterMaxLength() != null) {
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate("characterMaxLength must be null for this data type")
                    .addPropertyNode("characterMaxLength").addConstraintViolation();
            return false;
        }
        return true;
    }

    private boolean areNumericAndCharFieldsNull(ColumnDataTypeDefinition dto, ConstraintValidatorContext context) {
        return areNumericFieldsNull(dto, context) && isCharFieldNull(dto, context);
    }
}
