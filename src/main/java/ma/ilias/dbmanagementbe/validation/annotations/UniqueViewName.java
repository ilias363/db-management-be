package ma.ilias.dbmanagementbe.validation.annotations;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import ma.ilias.dbmanagementbe.validation.validators.UniqueViewNameValidator;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = UniqueViewNameValidator.class)
public @interface UniqueViewName {
    String message() default "View name already exists in this schema";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
