package ma.ilias.dbmanagementbe.validation;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = UniqueRoleNameValidator.class)
public @interface UniqueRoleName {
    String message() default "Role name already exists";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
