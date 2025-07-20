package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnPrimaryKeyDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingColumns;

@AllArgsConstructor
public class ExistingColumnsValidator implements ConstraintValidator<ExistingColumns, UpdateColumnPrimaryKeyDto> {

    private ColumnService columnService;

    @Override
    public boolean isValid(UpdateColumnPrimaryKeyDto colDto, ConstraintValidatorContext context) {
        if (colDto == null ||
                ValidationUtils.hasNullOrBlankValues(
                        colDto.getSchemaName(),
                        colDto.getTableName())) {
            return true;
        }

        try {
            if (colDto.getColumnNames().stream().anyMatch(
                    colName -> !columnService.columnExists(colDto.getSchemaName(), colDto.getTableName(), colName))) {
                ValidationUtils.addConstraintViolation(context,
                        "One or more columns does not exist in the specified table",
                        "columnNames");
                return false;
            }
            return true;
        } catch (

        TableNotFoundException ex) {
            ValidationUtils.addConstraintViolation(context,
                    "Table does not exist in the specified schema",
                    "tableName");
            return false;
        } catch (SchemaNotFoundException ex) {
            ValidationUtils.addConstraintViolation(context,
                    "Schema does not exist",
                    "schemaName");
            return false;
        }
    }
}
