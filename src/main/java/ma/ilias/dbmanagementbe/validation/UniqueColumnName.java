package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = UniqueColumnNameValidator.class)
public @interface UniqueColumnName {
    String message() default "Column name must be unique within the table";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}