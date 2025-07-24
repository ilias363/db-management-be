package ma.ilias.dbmanagementbe.validation.annotations;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import ma.ilias.dbmanagementbe.validation.validators.ValidFilterOperatorValidator;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = ValidFilterOperatorValidator.class)
public @interface ValidFilterOperator {
    String message() default "Invalid filter operator";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
