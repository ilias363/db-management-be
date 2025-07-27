package ma.ilias.dbmanagementbe.metadata.dto.view;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import ma.ilias.dbmanagementbe.metadata.dto.common.IViewReference;
import ma.ilias.dbmanagementbe.validation.annotations.ExistingView;
import ma.ilias.dbmanagementbe.validation.annotations.UniqueViewName;

@Data
@UniqueViewName
@ExistingView
public class UpdateViewDto implements IViewReference {
    @NotBlank(message = "Schema name cannot be blank")
    // schema existence checked in @UniqueViewName
    private String schemaName;

    @NotBlank(message = "View name cannot be blank")
    private String viewName;

    @NotBlank(message = "Updated view name cannot be blank")
    @Pattern(
            regexp = "^[a-zA-Z][a-zA-Z0-9_]*$",
            message = "View name must start with a letter and contain only alphanumeric characters and underscores"
    )
    private String updatedViewName;
}
