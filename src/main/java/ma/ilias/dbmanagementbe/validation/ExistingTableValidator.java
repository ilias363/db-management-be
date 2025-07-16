package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.common.ITableReference;
import ma.ilias.dbmanagementbe.metadata.service.table.TableService;
import org.springframework.beans.factory.annotation.Autowired;

public class ExistingTableValidator implements ConstraintValidator<ExistingTable, ITableReference> {

    @Autowired
    private TableService tableService;

    @Override
    public boolean isValid(ITableReference dto, ConstraintValidatorContext context) {
        if (dto == null || dto.getSchemaName() == null || dto.getTableName() == null) {
            return true;
        }

        try {
            return tableService.tableExists(dto.getSchemaName(), dto.getTableName());
        } catch (SchemaNotFoundException ex) {
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate("Schema does not exist")
                    .addPropertyNode("schemaName").addConstraintViolation();
            return false;
        }
    }
}
