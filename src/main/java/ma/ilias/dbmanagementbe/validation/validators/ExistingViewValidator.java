package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.common.IViewReference;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingView;

@AllArgsConstructor
public class ExistingViewValidator implements ConstraintValidator<ExistingView, IViewReference> {

    private MetadataProviderService metadataProviderService;

    @Override
    public boolean isValid(IViewReference dto, ConstraintValidatorContext context) {
        if (dto == null ||
                ValidationUtils.hasNullOrBlankValues(
                        dto.getSchemaName(),
                        dto.getViewName())) {
            return true;
        }

        try {
            return metadataProviderService.viewExists(dto.getSchemaName(), dto.getViewName());
        } catch (SchemaNotFoundException ex) {
            ValidationUtils.addConstraintViolation(context,
                    "Schema does not exist",
                    "schemaName");
            return false;
        }
    }
}
