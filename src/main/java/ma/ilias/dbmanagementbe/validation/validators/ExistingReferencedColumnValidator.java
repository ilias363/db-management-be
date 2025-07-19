package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.common.IReferencedColumnReference;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingReferencedColumn;
import org.springframework.beans.factory.annotation.Autowired;

public class ExistingReferencedColumnValidator implements ConstraintValidator<ExistingReferencedColumn, IReferencedColumnReference> {

    @Autowired
    private ColumnService columnService;

    @Override
    public boolean isValid(IReferencedColumnReference dto, ConstraintValidatorContext context) {
        if (dto == null ||
                dto.getReferencedSchemaName() == null ||
                dto.getReferencedTableName() == null ||
                dto.getReferencedColumnName() == null
        ) {
            return true;
        }

        try {
            if (!columnService.columnExists(
                    dto.getReferencedSchemaName(),
                    dto.getReferencedTableName(),
                    dto.getReferencedColumnName()
            )) {
                return false;
            }

            if (!columnService.isColumnPrimaryKey(
                    dto.getReferencedSchemaName(),
                    dto.getReferencedTableName(),
                    dto.getReferencedColumnName()
            )) {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate("Referenced column is not a primary key")
                        .addPropertyNode("referencedColumnName").addConstraintViolation();
                return false;
            }
            return true;
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
