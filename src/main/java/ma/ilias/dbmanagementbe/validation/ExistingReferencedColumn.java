package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = ExistingReferencedColumnValidator.class)
public @interface ExistingReferencedColumn {
    String message() default "Referenced column does not exist in the specified  referenced table";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
