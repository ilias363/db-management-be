package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.common.IReferencedColumnReference;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingReferencedColumn;

@AllArgsConstructor
public class ExistingReferencedColumnValidator
        implements ConstraintValidator<ExistingReferencedColumn, IReferencedColumnReference> {

    private MetadataProviderService metadataProviderService;

    @Override
    public boolean isValid(IReferencedColumnReference dto, ConstraintValidatorContext context) {
        if (dto == null ||
                ValidationUtils.hasNullOrBlankValues(
                        dto.getReferencedSchemaName(),
                        dto.getReferencedTableName(),
                        dto.getReferencedColumnName())) {
            return true;
        }

        try {
            if (!metadataProviderService.columnExists(
                    dto.getReferencedSchemaName(),
                    dto.getReferencedTableName(),
                    dto.getReferencedColumnName())) {
                return false;
            }

            if (!metadataProviderService.isColumnPrimaryKey(
                    dto.getReferencedSchemaName(),
                    dto.getReferencedTableName(),
                    dto.getReferencedColumnName())) {
                ValidationUtils.addConstraintViolation(context,
                        "Referenced column is not a primary key",
                        "referencedColumnName");
                return false;
            }
            return true;
        } catch (TableNotFoundException ex) {
            ValidationUtils.addConstraintViolation(context,
                    "Referenced table does not exist in the specified referenced schema",
                    "referencedTableName");
            return false;
        } catch (SchemaNotFoundException ex) {
            ValidationUtils.addConstraintViolation(context,
                    "Referenced schema does not exist",
                    "referencedSchemaName");
            return false;
        }
    }
}
