package ma.ilias.dbmanagementbe.validation.annotations;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import ma.ilias.dbmanagementbe.validation.validators.ValidAutoIncrementValidator;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = ValidAutoIncrementValidator.class)
public @interface ValidAutoIncrement {
    String message() default "Auto increment is only valid for numeric data types.";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
