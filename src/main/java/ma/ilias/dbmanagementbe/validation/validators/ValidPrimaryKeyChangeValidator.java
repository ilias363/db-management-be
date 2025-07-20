package ma.ilias.dbmanagementbe.validation.validators;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.enums.ColumnType;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.metadata.dto.column.update.UpdateColumnPrimaryKeyDto;
import ma.ilias.dbmanagementbe.metadata.service.column.ColumnService;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ValidPrimaryKeyChange;

@RequiredArgsConstructor
public class ValidPrimaryKeyChangeValidator
        implements ConstraintValidator<ValidPrimaryKeyChange, UpdateColumnPrimaryKeyDto> {

    private final ColumnService columnService;

    @Override
    public boolean isValid(UpdateColumnPrimaryKeyDto dto, ConstraintValidatorContext context) {
        if (dto == null || dto.getIsPrimaryKey() == null) {
            return true;
        }

        try {
            List<BaseColumnMetadataDto> currentColumns = dto.getColumnNames().stream()
                    .map(colName -> columnService.getColumn(dto.getSchemaName(), dto.getTableName(), colName))
                    .collect(Collectors.toList());

            boolean isCurrentlyPrimaryKey = currentColumns.stream().anyMatch(col -> Set
                    .of(ColumnType.PRIMARY_KEY, ColumnType.PRIMARY_KEY_FOREIGN_KEY).contains(col.getColumnType()));

            boolean isCurrentlyNullable = currentColumns.stream().anyMatch(col -> col.getIsNullable() != null && col.getIsNullable());

            if (isCurrentlyPrimaryKey == dto.getIsPrimaryKey()) {
                ValidationUtils.addConstraintViolation(context,
                        "One of the columns is already " + (isCurrentlyPrimaryKey ? "a primary key" : "not a primary key"),
                        null);
                return false;
            }

            if (dto.getIsPrimaryKey() && isCurrentlyNullable) {
                ValidationUtils.addConstraintViolation(context,
                        "Cannot add primary key constraint to nullable columns. Make columns NOT NULL first.",
                        null);
                return false;
            }

            // If removing PK constraint, check if there are referencing foreign keys
            // It will be checked in the service layer for referencing constraints
        } catch (Exception e) {
            return true;
        }

        return true;
    }
}
