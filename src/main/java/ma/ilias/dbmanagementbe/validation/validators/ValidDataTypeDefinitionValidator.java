package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnDataTypeDefinition;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ValidDataTypeDefinition;

public class ValidDataTypeDefinitionValidator
        implements ConstraintValidator<ValidDataTypeDefinition, IColumnDataTypeDefinition> {

    @Override
    public boolean isValid(IColumnDataTypeDefinition dto, ConstraintValidatorContext context) {
        if (dto == null || dto.getDataType() == null) {
            return true; // Handled by @NotBlank on the field itself
        }

        String dataType = dto.getDataType().toUpperCase();

        return switch (dataType) {
            case "VARCHAR", "CHAR" -> ValidationUtils.validateVarcharOrChar(dto, context);
            case "DECIMAL", "NUMERIC" -> ValidationUtils.validateDecimalOrNumeric(dto, context);
            case "INT", "INTEGER", "SMALLINT", "BIGINT", "BOOLEAN", "DATE", "TIME", "TIMESTAMP", "TEXT", "FLOAT",
                 "REAL", "DOUBLE" -> ValidationUtils.validateNumericAndCharFieldsNull(dto, context);
            default -> true; // The @Pattern annotation will handle unknown types
        };
    }
}
