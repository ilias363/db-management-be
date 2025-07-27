package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.dto.permission.PermissionDetailDto;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ValidPermissionFields;

@AllArgsConstructor
public class ValidPermissionFieldsValidator implements ConstraintValidator<ValidPermissionFields, PermissionDetailDto> {

    private MetadataProviderService metadataProviderService;

    @Override
    public boolean isValid(PermissionDetailDto dto, ConstraintValidatorContext context) {
        if (dto == null) {
            return true;
        }

        try {
            boolean hasSchema = dto.getSchemaName() != null && !dto.getSchemaName().isBlank();
            boolean hasTable = dto.getTableName() != null && !dto.getTableName().isBlank();
            boolean hasView = dto.getViewName() != null && !dto.getViewName().isBlank();

            if (hasTable && hasView) {
                ValidationUtils.addConstraintViolation(context, "Cannot specify table and view at the same time", "tableName");
                return false;
            }

            if (!hasSchema && hasTable) {
                ValidationUtils.addConstraintViolation(context, "Cannot specify table without schema", "tableName");
                return false;
            }

            if (!hasSchema && hasView) {
                ValidationUtils.addConstraintViolation(context, "Cannot specify view without schema", "viewName");
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

            if (hasView && !metadataProviderService.viewExists(dto.getSchemaName(), dto.getViewName())) {
                ValidationUtils.addConstraintViolation(context, "Table does not exist", "tableName");
                return false;
            }

            return true;
        } catch (SchemaNotFoundException ex) {
            ValidationUtils.addConstraintViolation(context, "Schema does not exist", "schemaName");
            return false;
        }
    }
}
