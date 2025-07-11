package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.table.NewTableDto;
import ma.ilias.dbmanagementbe.metadata.dto.table.TableDtoBase;
import ma.ilias.dbmanagementbe.metadata.dto.table.UpdateTableDto;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaService;
import ma.ilias.dbmanagementbe.metadata.service.table.TableService;
import org.springframework.beans.factory.annotation.Autowired;

public class UniqueTableNameValidator implements ConstraintValidator<UniqueTableName, TableDtoBase> {

    @Autowired
    private SchemaService schemaService;

    @Autowired
    private TableService tableService;

    @Override
    public boolean isValid(TableDtoBase dto, ConstraintValidatorContext context) {
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
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate("Schema does not exist")
                    .addPropertyNode("schemaName").addConstraintViolation();
            return false;
        }
    }
}
