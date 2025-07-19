package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.validation.ValidationUtils;
import ma.ilias.dbmanagementbe.validation.annotations.ValidForeignKeyDefault;
import org.springframework.jdbc.core.JdbcTemplate;

@RequiredArgsConstructor
public class ValidForeignKeyDefaultValidator
        implements ConstraintValidator<ValidForeignKeyDefault, NewForeignKeyColumnDto> {

    private final JdbcTemplate jdbcTemplate;

    @Override
    public boolean isValid(NewForeignKeyColumnDto dto, ConstraintValidatorContext context) {
        if (dto == null ||
                ValidationUtils.hasNullOrBlankValues(dto.getColumnDefault(),
                        dto.getReferencedSchemaName(),
                        dto.getReferencedTableName(),
                        dto.getReferencedColumnName())) {
            return true;
        }

        return ValidationUtils.validateValueExistsInReferencedTable(
                jdbcTemplate,
                dto.getReferencedSchemaName(),
                dto.getReferencedTableName(),
                dto.getReferencedColumnName(),
                dto.getColumnDefault(),
                context,
                "columnDefault");
    }
}
