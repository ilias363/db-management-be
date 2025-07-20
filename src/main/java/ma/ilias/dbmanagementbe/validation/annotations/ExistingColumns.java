package ma.ilias.dbmanagementbe.validation.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import ma.ilias.dbmanagementbe.validation.validators.ExistingColumnsValidator;

@Constraint(validatedBy = ExistingColumnsValidator.class)
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface ExistingColumns {
    String message() default "One or more columns does not exist";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
