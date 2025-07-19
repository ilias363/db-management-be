package ma.ilias.dbmanagementbe.validation.annotations;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import ma.ilias.dbmanagementbe.validation.validators.ExistingRolesValidator;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = ExistingRolesValidator.class)
public @interface ExistingRoles {
    String message() default "One or more roles do not exist";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
