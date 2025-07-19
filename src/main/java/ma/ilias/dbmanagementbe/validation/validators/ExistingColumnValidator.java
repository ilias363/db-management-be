package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnReference;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingColumn;

@AllArgsConstructor
public class ExistingColumnValidator implements ConstraintValidator<ExistingColumn, IColumnReference> {

    private ColumnService columnService;

    @Override
    public boolean isValid(IColumnReference colDto, ConstraintValidatorContext context) {
        if (colDto == null ||
                ValidationUtils.hasNullOrBlankValues(
                        colDto.getSchemaName(),
                        colDto.getTableName(),
                        colDto.getColumnName())) {
            return true;
        }

        try {
            if (!columnService.columnExists(colDto.getSchemaName(), colDto.getTableName(), colDto.getColumnName())) {
                ValidationUtils.addConstraintViolation(context,
                        "Column does not exist in the specified table",
                        "columnName");
                return false;
            }
            return true;
        } catch (TableNotFoundException ex) {
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
