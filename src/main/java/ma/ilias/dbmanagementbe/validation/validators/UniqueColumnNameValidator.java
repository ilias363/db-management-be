package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.RenameColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnReference;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueColumnName;
import org.springframework.beans.factory.annotation.Autowired;

public class UniqueColumnNameValidator implements ConstraintValidator<UniqueColumnName, IColumnReference> {

    @Autowired
    private ColumnService columnService;

    @Override
    public boolean isValid(IColumnReference dto, ConstraintValidatorContext context) {
        if (dto == null ||
                dto.getSchemaName() == null ||
                dto.getTableName() == null ||
                dto.getColumnName() == null) {
            return true;
        }
        try {
            if (dto instanceof BaseNewColumnDto newDto) {
                return !columnService.columnExists(newDto.getSchemaName(), newDto.getTableName(), newDto.getColumnName());
            }

            if (dto instanceof RenameColumnDto renameDto) {
                if (renameDto.getNewColumnName() == null) {
                    return true;
                }
                return !columnService.columnExists(renameDto.getSchemaName(), renameDto.getTableName(), renameDto.getNewColumnName());
            }
            return true;
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
