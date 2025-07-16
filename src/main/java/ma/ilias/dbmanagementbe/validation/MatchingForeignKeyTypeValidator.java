package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.exception.ColumnNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;

@RequiredArgsConstructor
public class MatchingForeignKeyTypeValidator implements ConstraintValidator<MatchingForeignKeyType, NewForeignKeyColumnDto> {

    private final ColumnService columnService;

    @Override
    public boolean isValid(NewForeignKeyColumnDto dto, ConstraintValidatorContext context) {
        if (dto == null ||
                dto.getReferencedSchemaName() == null ||
                dto.getReferencedTableName() == null ||
                dto.getReferencedColumnName() == null ||
                dto.getDataType() == null) {
            return true;
        }

        boolean isValid;
        context.disableDefaultConstraintViolation();

        try {
            BaseColumnMetadataDto referencedColumn = columnService.getColumn(
                    dto.getReferencedSchemaName(),
                    dto.getReferencedTableName(),
                    dto.getReferencedColumnName()
            );

            // Check if data types match
            if (!referencedColumn.getDataType().equalsIgnoreCase(dto.getDataType())) {
                context.buildConstraintViolationWithTemplate(
                                String.format("Data type '%s' does not match referenced column's data type '%s'",
                                        dto.getDataType(), referencedColumn.getDataType()))
                        .addPropertyNode("dataType")
                        .addConstraintViolation();
                isValid = false;
            } else {
                // Only check additional properties if data types match
                isValid = validateDataTypeProperties(dto, referencedColumn, context);
            }

        } catch (ColumnNotFoundException e) {
            // This case is handled by another validator
            return true;
        }

        return isValid;
    }

    private boolean validateDataTypeProperties(NewForeignKeyColumnDto dto, BaseColumnMetadataDto referencedColumn,
                                               ConstraintValidatorContext context) {
        boolean isValid = true;
        String dataType = dto.getDataType().toUpperCase();

        switch (dataType) {
            case "VARCHAR", "CHAR" -> isValid = validateCharacterProperties(dto, referencedColumn, context);
            case "DECIMAL", "NUMERIC" -> isValid = validateNumericProperties(dto, referencedColumn, context);
        }

        return isValid;
    }

    private boolean validateCharacterProperties(NewForeignKeyColumnDto dto, BaseColumnMetadataDto referencedColumn,
                                                ConstraintValidatorContext context) {
        boolean isValid = true;

        Long dtoCharLength = dto.getCharacterMaxLength() != null ? dto.getCharacterMaxLength().longValue() : null;
        if (!isLongEqual(dtoCharLength, referencedColumn.getCharacterMaxLength())) {
            context.buildConstraintViolationWithTemplate(
                            String.format("Character max length '%s' does not match referenced column's character max length '%s'",
                                    dto.getCharacterMaxLength(), referencedColumn.getCharacterMaxLength()))
                    .addPropertyNode("characterMaxLength")
                    .addConstraintViolation();
            isValid = false;
        }

        return isValid;
    }

    private boolean validateNumericProperties(NewForeignKeyColumnDto dto, BaseColumnMetadataDto referencedColumn,
                                              ConstraintValidatorContext context) {
        boolean isValid = true;

        if (!isIntegerEqual(dto.getNumericPrecision(), referencedColumn.getNumericPrecision())) {
            context.buildConstraintViolationWithTemplate(
                            String.format("Numeric precision '%s' does not match referenced column's numeric precision '%s'",
                                    dto.getNumericPrecision(), referencedColumn.getNumericPrecision()))
                    .addPropertyNode("numericPrecision")
                    .addConstraintViolation();
            isValid = false;
        }

        if (!isIntegerEqual(dto.getNumericScale(), referencedColumn.getNumericScale())) {
            context.buildConstraintViolationWithTemplate(
                            String.format("Numeric scale '%s' does not match referenced column's numeric scale '%s'",
                                    dto.getNumericScale(), referencedColumn.getNumericScale()))
                    .addPropertyNode("numericScale")
                    .addConstraintViolation();
            isValid = false;
        }

        return isValid;
    }

    private boolean isIntegerEqual(Integer value1, Integer value2) {
        if (value1 == null && value2 == null) return true;
        if (value1 == null || value2 == null) return false;
        return value1.equals(value2);
    }

    private boolean isLongEqual(Long value1, Long value2) {
        if (value1 == null && value2 == null) return true;
        if (value1 == null || value2 == null) return false;
        return value1.equals(value2);
    }
}
