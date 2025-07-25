package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.exception.SchemaNotFoundException;
import ma.ilias.dbmanagementbe.exception.TableNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseNewColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.RenameColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnReference;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueColumnName;

@AllArgsConstructor
public class UniqueColumnNameValidator implements ConstraintValidator<UniqueColumnName, IColumnReference> {

    private MetadataProviderService metadataProviderService;

    @Override
    public boolean isValid(IColumnReference dto, ConstraintValidatorContext context) {
        if (dto == null ||
                ValidationUtils.hasNullOrBlankValues(
                        dto.getSchemaName(),
                        dto.getTableName(),
                        dto.getColumnName())) {
            return true;
        }

        try {
            if (dto instanceof BaseNewColumnDto newDto) {
                return !metadataProviderService.columnExists(newDto.getSchemaName(), newDto.getTableName(),
                        newDto.getColumnName());
            }

            if (dto instanceof RenameColumnDto renameDto) {
                if (renameDto.getNewColumnName() == null) {
                    return true;
                }
                return !metadataProviderService.columnExists(renameDto.getSchemaName(), renameDto.getTableName(),
                        renameDto.getNewColumnName());
            }
            return true;
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
