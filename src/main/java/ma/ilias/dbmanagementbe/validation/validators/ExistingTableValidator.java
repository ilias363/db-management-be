package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.common.ITableReference;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingTable;

@AllArgsConstructor
public class ExistingTableValidator implements ConstraintValidator<ExistingTable, ITableReference> {

    private MetadataProviderService metadataProviderService;

    @Override
    public boolean isValid(ITableReference dto, ConstraintValidatorContext context) {
        if (dto == null ||
                ValidationUtils.hasNullOrBlankValues(
                        dto.getSchemaName(),
                        dto.getTableName())) {
            return true;
        }

        try {
            return metadataProviderService.tableExists(dto.getSchemaName(), dto.getTableName());
        } catch (SchemaNotFoundException ex) {
            ValidationUtils.addConstraintViolation(context,
                    "Schema does not exist",
                    "schemaName");
            return false;
        }
    }
}
