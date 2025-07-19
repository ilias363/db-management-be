package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaService;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueSchemaName;
import org.springframework.beans.factory.annotation.Autowired;

public class UniqueSchemaNameValidator implements ConstraintValidator<UniqueSchemaName, String> {

    @Autowired
    private SchemaService schemaService;

    @Override
    public boolean isValid(String schemaName, ConstraintValidatorContext context) {
        if (schemaName == null || schemaName.isBlank()) {
            return true; // Not the responsibility of this validator
        }
        return !schemaService.schemaExists(schemaName);
    }
}
