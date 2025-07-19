package ma.ilias.dbmanagementbe.validation.validators;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import ma.ilias.dbmanagementbe.metadata.dto.column.foreignkey.NewForeignKeyColumnDto;
import ma.ilias.dbmanagementbe.validation.annotations.ValidForeignKeyDefault;
import org.springframework.jdbc.core.JdbcTemplate;

@RequiredArgsConstructor
public class ValidForeignKeyDefaultValidator implements ConstraintValidator<ValidForeignKeyDefault, NewForeignKeyColumnDto> {

    private final JdbcTemplate jdbcTemplate;

    @Override
    public boolean isValid(NewForeignKeyColumnDto dto, ConstraintValidatorContext context) {
        if (dto == null ||
                dto.getColumnDefault() == null ||
                dto.getColumnDefault().trim().isEmpty() ||
                dto.getReferencedSchemaName() == null ||
                dto.getReferencedTableName() == null ||
                dto.getReferencedColumnName() == null
        ) {
            return true;
        }

        try {
            String checkSql = String.format(
                    "SELECT COUNT(*) FROM %s.%s WHERE %s = ?",
                    dto.getReferencedSchemaName(),
                    dto.getReferencedTableName(),
                    dto.getReferencedColumnName()
            );

            Integer count = jdbcTemplate.queryForObject(checkSql, Integer.class, dto.getColumnDefault());

            if (count == null || count == 0) {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate(
                                String.format("Default value '%s' does not exist in the referenced table '%s.%s' column '%s'",
                                        dto.getColumnDefault(),
                                        dto.getReferencedSchemaName(),
                                        dto.getReferencedTableName(),
                                        dto.getReferencedColumnName()))
                        .addPropertyNode("columnDefault")
                        .addConstraintViolation();
                return false;
            }

            return true;
        } catch (Exception e) {
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate(
                            "Unable to validate default value in referenced table")
                    .addPropertyNode("columnDefault")
                    .addConstraintViolation();
            return false;
        }
    }
}
