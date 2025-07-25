package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.index.NewIndexDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueIndexName;

@RequiredArgsConstructor
public class UniqueIndexNameValidator implements ConstraintValidator<UniqueIndexName, NewIndexDto> {

    private final MetadataProviderService metadataProviderService;

    @Override
    public boolean isValid(NewIndexDto dto, ConstraintValidatorContext context) {
        if (dto == null ||
                ValidationUtils.hasNullOrBlankValues(
                        dto.getSchemaName(),
                        dto.getTableName(),
                        dto.getIndexName())) {
            return true;
        }

        try {
            return !metadataProviderService.indexExists(dto.getSchemaName(), dto.getTableName(), dto.getIndexName());
        } catch (TableNotFoundException ex) {
            ValidationUtils.addConstraintViolation(context,
                    "Table does not exist",
                    "tableName");
            return false;
        } catch (SchemaNotFoundException ex) {
            ValidationUtils.addConstraintViolation(context,
                    "Schema does not exist",
                    "schemaName");
            return false;
        }
    }
}
