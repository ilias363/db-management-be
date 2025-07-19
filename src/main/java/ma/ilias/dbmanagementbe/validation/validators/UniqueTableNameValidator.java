package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.common.ITableReference;
import ma.ilias.dbmanagementbe.metadata.dto.table.NewTableDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.UpdateTableDto;
import ma.ilias.dbmanagementbe.metadata.service.table.TableService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueTableName;
import org.springframework.beans.factory.annotation.Autowired;

public class UniqueTableNameValidator implements ConstraintValidator<UniqueTableName, ITableReference> {

    @Autowired
    private TableService tableService;

    @Override
    public boolean isValid(ITableReference dto, ConstraintValidatorContext context) {
        if (dto.getSchemaName() == null) {
            return true;
        }

        try {
            // For NewTableDto
            if (dto instanceof NewTableDto newDto) {
                if (newDto.getTableName() == null) {
                    return true;
                }
                return !tableService.tableExists(newDto.getSchemaName(), newDto.getTableName());
            }

            // For UpdateTableDto
            if (dto instanceof UpdateTableDto updateDto) {
                if (updateDto.getUpdatedTableName() == null) {
                    return true;
                }
                return !tableService.tableExists(updateDto.getSchemaName(), updateDto.getUpdatedTableName());
            }
            return true;
        } catch (SchemaNotFoundException ex) {
            ValidationUtils.addConstraintViolation(context,
                    "Schema does not exist",
                    "schemaName");
            return false;
        }
    }
}
