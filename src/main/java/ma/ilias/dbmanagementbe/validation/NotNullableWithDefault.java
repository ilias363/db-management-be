package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Constraint(validatedBy = NotNullableWithDefaultValidator.class)
@Target({ ElementType.TYPE })
@Retention(RetentionPolicy.RUNTIME)
public @interface NotNullableWithDefault {
    String message() default "A default value must be provided when making a column non-nullable.";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
