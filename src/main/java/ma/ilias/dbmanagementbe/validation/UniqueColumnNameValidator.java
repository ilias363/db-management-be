package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.IColumnReference;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import org.springframework.beans.factory.annotation.Autowired;

public class UniqueColumnNameValidator implements ConstraintValidator<UniqueColumnName, IColumnReference> {

    @Autowired
    private ColumnService columnService;

    @Override
    public boolean isValid(IColumnReference dto, ConstraintValidatorContext context) {
        if (dto == null || dto.getSchemaName() == null || dto.getTableName() == null || dto.getColumnName() == null) {
            return true; // Let other validators handle nulls
        }
        try {
            return !columnService.columnExists(dto.getSchemaName(), dto.getTableName(), dto.getColumnName());
        } catch (TableNotFoundException ex) {
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate("Table does not exist")
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
