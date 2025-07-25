package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueSchemaName;

@AllArgsConstructor
public class UniqueSchemaNameValidator implements ConstraintValidator<UniqueSchemaName, String> {

    private MetadataProviderService metadataProviderService;

    @Override
    public boolean isValid(String schemaName, ConstraintValidatorContext context) {
        if (schemaName == null || schemaName.isBlank()) {
            return true; // Not the responsibility of this validator
        }
        return !metadataProviderService.schemaExists(schemaName);
    }
}
