package ma.ilias.dbmanagementbe.validation.annotations;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import ma.ilias.dbmanagementbe.validation.validators.MatchingForeignKeyTypeValidator;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Constraint(validatedBy = MatchingForeignKeyTypeValidator.class)
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface MatchingForeignKeyType {
    String message() default "Foreign key column definition must match the referenced column's definition (data type, length, precision, scale)";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
