package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnReference;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;

@AllArgsConstructor
public class ExistingColumnValidator implements ConstraintValidator<ExistingColumn, IColumnReference> {

    private ColumnService columnService;

    @Override
    public boolean isValid(IColumnReference colDto, ConstraintValidatorContext context) {
        if (colDto == null ||
                colDto.getSchemaName() == null ||
                colDto.getTableName() == null ||
                colDto.getColumnName() == null) {
            return true;
        }

        try {
            if (!columnService.columnExists(colDto.getSchemaName(), colDto.getTableName(), colDto.getColumnName())) {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate("Column does not exist in the specified table")
                        .addPropertyNode("columnName").addConstraintViolation();
                return false;
            }
            return true;
        } catch (TableNotFoundException ex) {
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate("Table does not exist in the specified schema")
                    .addPropertyNode("tableName").addConstraintViolation();
            return false;
        } catch (SchemaNotFoundException ex) {
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate("Schema does not exist")
                    .addPropertyNode("schemaName").addConstraintViolation();
            return false;
        }
    }
}
