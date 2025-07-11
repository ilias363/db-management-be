package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import ma.ilias.dbmanagementbe.metadata.service.schema.SchemaService;
import org.springframework.beans.factory.annotation.Autowired;

public class ExistingSchemaValidator implements ConstraintValidator<ExistingSchema, String> {

    @Autowired
    private SchemaService schemaService;

    @Override
    public boolean isValid(String schemaName, ConstraintValidatorContext context) {

        return schemaName == null ||
                schemaName.isBlank() ||
                schemaService.schemaExists(schemaName);
    }
}
