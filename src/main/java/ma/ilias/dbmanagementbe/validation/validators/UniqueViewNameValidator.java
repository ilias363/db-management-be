package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.common.IViewReference;
import ma.ilias.dbmanagementbe.metadata.dto.view.UpdateViewDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueViewName;

@AllArgsConstructor
public class UniqueViewNameValidator implements ConstraintValidator<UniqueViewName, IViewReference> {

    private MetadataProviderService metadataProviderService;

    @Override
    public boolean isValid(IViewReference dto, ConstraintValidatorContext context) {
        if (dto.getSchemaName() == null) {
            return true;
        }

        try {
            // For NewViewDto to add later
//            if (dto instanceof NewViewDto newDto) {
//                if (newDto.getViewName() == null) {
//                    return true;
//                }
//                return !metadataProviderService.tableOrViewExists(newDto.getSchemaName(), newDto.getViewName());
//            }

            // For UpdateViewDto
            if (dto instanceof UpdateViewDto updateDto) {
                if (updateDto.getUpdatedViewName() == null) {
                    return true;
                }
                return !metadataProviderService.tableOrViewExists(updateDto.getSchemaName(), updateDto.getUpdatedViewName());
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
