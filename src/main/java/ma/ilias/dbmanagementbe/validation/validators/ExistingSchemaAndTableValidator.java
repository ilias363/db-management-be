package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.dto.permission.NewPermissionDto;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaService;
import ma.ilias.dbmanagementbe.metadata.service.table.TableService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingSchemaAndTable;
import org.springframework.beans.factory.annotation.Autowired;

public class ExistingSchemaAndTableValidator implements ConstraintValidator<ExistingSchemaAndTable, NewPermissionDto> {

    @Autowired
    private SchemaService schemaService;

    @Autowired
    private TableService tableService;

    @Override
    public boolean isValid(NewPermissionDto dto, ConstraintValidatorContext context) {
        if (dto == null) {
            return true;
        }

        try {
            boolean hasSchema = dto.getSchemaName() != null && !dto.getSchemaName().isBlank();
            boolean hasTable = dto.getTableName() != null && !dto.getTableName().isBlank();

            if (!hasSchema && hasTable) {
                ValidationUtils.addConstraintViolation(context, "Cannot specify table without schema", "tableName");
                return false;
            }

            if (hasSchema && !schemaService.schemaExists(dto.getSchemaName())) {
                ValidationUtils.addConstraintViolation(context, "Schema does not exist", "schemaName");
                return false;
            }

            if (hasTable && !tableService.tableExists(dto.getSchemaName(), dto.getTableName())) {
                ValidationUtils.addConstraintViolation(context, "Table does not exist", "tableName");
                return false;
            }

            return true;
        } catch (SchemaNotFoundException ex) {
            ValidationUtils.addConstraintViolation(context, "Schema does not exist", "schemaName");
            return false;
        } catch (TableNotFoundException ex) {
            ValidationUtils.addConstraintViolation(context, "Table does not exist", "tableName");
            return false;
        }
    }
}
