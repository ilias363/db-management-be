package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.primarykey.NewPrimaryKeyColumnDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnAutoIncrementDto;
import ma.ilias.dbmanagementbe.metadata.dto.common.IColumnReference;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ValidAutoIncrement;

@AllArgsConstructor
public class ValidAutoIncrementValidator implements ConstraintValidator<ValidAutoIncrement, IColumnReference> {

    private ColumnService columnService;

    @Override
    public boolean isValid(IColumnReference dto, ConstraintValidatorContext context) {
        if (dto == null) {
            return true;
        }

        Boolean autoIncrement = null;
        String dataType = null;

        if (dto instanceof NewPrimaryKeyColumnDto newDto) {
            autoIncrement = newDto.getAutoIncrement();
            dataType = newDto.getDataType();
        } else if (dto instanceof UpdateColumnAutoIncrementDto updateDto) {
            autoIncrement = updateDto.getAutoIncrement();
            if (autoIncrement != null && autoIncrement) {
                try {
                    var currentColumn = columnService.getColumn(
                            updateDto.getSchemaName(),
                            updateDto.getTableName(),
                            updateDto.getColumnName(),
                            false, false);
                    if (currentColumn.getColumnType() != ColumnType.PRIMARY_KEY) {
                        ValidationUtils.addConstraintViolation(context,
                                "Auto-increment can only be used with primary key columns",
                                null);
                        return false;
                    }
                    dataType = currentColumn != null ? currentColumn.getDataType() : null;
                } catch (Exception e) {
                    return true;
                }
            }
        }

        if (autoIncrement == null || !autoIncrement || dataType == null) {
            return true;
        }

        return ValidationUtils.isAutoIncrementCompatible(dataType);
    }
}
