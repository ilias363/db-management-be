package ma.ilias.dbmanagementbe.validation.annotations;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import ma.ilias.dbmanagementbe.validation.validators.NoDuplicateColumnNamesValidator;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = NoDuplicateColumnNamesValidator.class)
public @interface NoDuplicateColumnNames {
    String message() default "Duplicate column names are not allowed";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
