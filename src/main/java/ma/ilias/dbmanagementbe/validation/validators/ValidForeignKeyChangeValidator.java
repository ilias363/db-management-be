package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnForeignKeyDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.annotations.ValidForeignKeyChange;

@RequiredArgsConstructor
public class ValidForeignKeyChangeValidator implements ConstraintValidator<ValidForeignKeyChange, UpdateColumnForeignKeyDto> {

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
                    dto.getColumnName()
            );

            boolean isCurrentlyForeignKey = currentColumn.getColumnType() == ColumnType.FOREIGN_KEY;
            if (isCurrentlyForeignKey == dto.getIsForeignKey()) {
                context.buildConstraintViolationWithTemplate("Column is already " +
                                (isCurrentlyForeignKey ? "a foreign key" : "not a foreign key"))
                        .addConstraintViolation();
                return false;
            }

            if (dto.getIsForeignKey()) {
                if (dto.getReferencedSchemaName() == null || dto.getReferencedSchemaName().isBlank()) {
                    context.buildConstraintViolationWithTemplate(
                                    "Referenced schema name is required when adding foreign key constraint")
                            .addPropertyNode("referencedSchemaName")
                            .addConstraintViolation();
                    return false;
                }

                if (dto.getReferencedTableName() == null || dto.getReferencedTableName().isBlank()) {
                    context.buildConstraintViolationWithTemplate(
                                    "Referenced table name is required when adding foreign key constraint")
                            .addPropertyNode("referencedTableName")
                            .addConstraintViolation();
                    return false;
                }

                if (dto.getReferencedColumnName() == null || dto.getReferencedColumnName().isBlank()) {
                    context.buildConstraintViolationWithTemplate(
                                    "Referenced column name is required when adding foreign key constraint")
                            .addPropertyNode("referencedColumnName")
                            .addConstraintViolation();
                    return false;
                }

                var referencedColumn = columnService.getColumn(
                        dto.getReferencedSchemaName(),
                        dto.getReferencedTableName(),
                        dto.getReferencedColumnName()
                );

                if (!currentColumn.getDataType().equalsIgnoreCase(referencedColumn.getDataType())) {
                    context.buildConstraintViolationWithTemplate(
                                    String.format("Data type '%s' does not match referenced column's data type '%s'",
                                            currentColumn.getDataType(), referencedColumn.getDataType()))
                            .addConstraintViolation();
                    return false;
                } else {
                    return validateDataTypeProperties(currentColumn, referencedColumn, context);
                }
            }

        } catch (Exception e) {
            return true;
        }

        return true;
    }

    private boolean validateDataTypeProperties(BaseColumnMetadataDto currentColumn, BaseColumnMetadataDto referencedColumn,
                                               ConstraintValidatorContext context) {
        boolean isValid = true;
        String dataType = currentColumn.getDataType().toUpperCase();

        switch (dataType) {
            case "VARCHAR", "CHAR" -> isValid = validateCharacterProperties(currentColumn, referencedColumn, context);
            case "DECIMAL", "NUMERIC" -> isValid = validateNumericProperties(currentColumn, referencedColumn, context);
        }

        return isValid;
    }

    private boolean validateCharacterProperties(BaseColumnMetadataDto currentColumn, BaseColumnMetadataDto referencedColumn,
                                                ConstraintValidatorContext context) {
        boolean isValid = true;

        Long dtoCharLength = currentColumn.getCharacterMaxLength() != null ? currentColumn.getCharacterMaxLength().longValue() : null;
        if (!isLongEqual(dtoCharLength, referencedColumn.getCharacterMaxLength())) {
            context.buildConstraintViolationWithTemplate(
                            String.format("Character max length '%s' does not match referenced column's character max length '%s'",
                                    currentColumn.getCharacterMaxLength(), referencedColumn.getCharacterMaxLength()))
                    .addConstraintViolation();
            isValid = false;
        }

        return isValid;
    }

    private boolean validateNumericProperties(BaseColumnMetadataDto currentColumn, BaseColumnMetadataDto referencedColumn,
                                              ConstraintValidatorContext context) {
        boolean isValid = true;

        if (!isIntegerEqual(currentColumn.getNumericPrecision(), referencedColumn.getNumericPrecision())) {
            context.buildConstraintViolationWithTemplate(
                            String.format("Numeric precision '%s' does not match referenced column's numeric precision '%s'",
                                    currentColumn.getNumericPrecision(), referencedColumn.getNumericPrecision()))
                    .addConstraintViolation();
            isValid = false;
        }

        if (!isIntegerEqual(currentColumn.getNumericScale(), referencedColumn.getNumericScale())) {
            context.buildConstraintViolationWithTemplate(
                            String.format("Numeric scale '%s' does not match referenced column's numeric scale '%s'",
                                    currentColumn.getNumericScale(), referencedColumn.getNumericScale()))
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
