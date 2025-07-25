package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.annotations.NotSystemSchema;

@RequiredArgsConstructor
public class NotSystemSchemaValidator implements ConstraintValidator<NotSystemSchema, String> {

    private final MetadataProviderService metadataProviderService;

    @Override
    public boolean isValid(String value, ConstraintValidatorContext context) {
        return !metadataProviderService.isSystemSchemaByName(value);
    }
}
