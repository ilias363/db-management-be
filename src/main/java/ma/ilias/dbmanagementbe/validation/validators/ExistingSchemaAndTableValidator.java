package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.permission.NewPermissionDto;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingSchemaAndTable;

@AllArgsConstructor
public class ExistingSchemaAndTableValidator implements ConstraintValidator<ExistingSchemaAndTable, NewPermissionDto> {

    private MetadataProviderService metadataProviderService;

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

            if (hasSchema && !metadataProviderService.schemaExists(dto.getSchemaName())) {
                ValidationUtils.addConstraintViolation(context, "Schema does not exist", "schemaName");
                return false;
            }

            if (hasTable && !metadataProviderService.tableExists(dto.getSchemaName(), dto.getTableName())) {
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
