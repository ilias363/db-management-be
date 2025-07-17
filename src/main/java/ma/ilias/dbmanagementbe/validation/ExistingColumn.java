package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Constraint(validatedBy = ExistingColumnValidator.class)
@Target({ ElementType.TYPE })
@Retention(RetentionPolicy.RUNTIME)
public @interface ExistingColumn {
    String message() default "Column does not exist";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
