package ma.ilias.dbmanagementbe.metadata.service.view;

import ma.ilias.dbmanagementbe.metadata.dto.view.ViewMetadataDto;

import java.util.List;

public interface ViewService {
    boolean viewExists(String schemaName, String viewName);

    ViewMetadataDto getView(String schemaName, String viewName, boolean includeSchema,
                            boolean includeColumns, boolean checkViewExists);

    ViewMetadataDto getView(String schemaName, String viewName, boolean includeSchema,
                            boolean includeColumns, boolean checkViewExists, boolean checkAuthorization);

    List<ViewMetadataDto> getViewsBySchema(String schemaName, boolean includeSchema,
                                           boolean includeColumns, boolean checkSchemaExists);

    Boolean deleteView(String schemaName, String viewName);
}
