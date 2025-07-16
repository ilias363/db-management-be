package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.IReferencedColumnReference;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import org.springframework.beans.factory.annotation.Autowired;

public class ExistingReferencedColumnValidator implements ConstraintValidator<ExistingReferencedColumn, IReferencedColumnReference> {

    @Autowired
    private ColumnService columnService;

    @Override
    public boolean isValid(IReferencedColumnReference dto, ConstraintValidatorContext context) {
        if (
                dto == null ||
                        dto.getReferencedColumnName() == null ||
                        dto.getReferencedTableName() == null ||
                        dto.getReferencedColumnName() == null
        ) {
            return true;
        }

        try {
            return columnService.columnExists(
                    dto.getReferencedSchemaName(),
                    dto.getReferencedTableName(),
                    dto.getReferencedColumnName()
            );
        } catch (TableNotFoundException ex) {
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate("Referenced table does not exist in the specified referenced schema")
                    .addPropertyNode("referencedTableName").addConstraintViolation();
            return false;
        } catch (SchemaNotFoundException ex) {
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate("Referenced schema does not exist")
                    .addPropertyNode("referencedSchemaName").addConstraintViolation();
            return false;
        }
    }
}
