package ma.ilias.dbmanagementbe.validation.annotations;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import ma.ilias.dbmanagementbe.validation.validators.NotNullOrUniqueValidator;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = NotNullOrUniqueValidator.class)
public @interface NotNullOrUnique {
    String message() default "A column cannot be both not nullable and unique";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
