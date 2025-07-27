package ma.ilias.dbmanagementbe.metadata.service.view;

import lombok.AllArgsConstructor;
import ma.ilias.dbmanagementbe.enums.PermissionType;
import ma.ilias.dbmanagementbe.exception.UnauthorizedActionException;
import ma.ilias.dbmanagementbe.exception.ViewNotFoundException;
import ma.ilias.dbmanagementbe.metadata.dto.view.UpdateViewDto;
import ma.ilias.dbmanagementbe.metadata.dto.view.ViewMetadataDto;
import ma.ilias.dbmanagementbe.metadata.service.MetadataProviderService;
import ma.ilias.dbmanagementbe.service.DatabaseAuthorizationService;
import ma.ilias.dbmanagementbe.util.AuthorizationUtils;
import ma.ilias.dbmanagementbe.util.SqlSecurityUtils;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
@Transactional
public class MySqlViewManager implements ViewService {

    private final JdbcTemplate jdbcTemplate;
    private final MetadataProviderService metadataProviderService;
    private final DatabaseAuthorizationService databaseAuthorizationService;

    @Override
    public boolean viewExists(String schemaName, String viewName) {
        return metadataProviderService.viewExists(schemaName, viewName);
    }

    @Override
    public ViewMetadataDto getView(String schemaName, String viewName, boolean includeSchema,
                                   boolean includeColumns, boolean checkViewExists) {
        return getView(schemaName, viewName, includeSchema, includeColumns, checkViewExists, false);
    }

    @Override
    public ViewMetadataDto getView(String schemaName, String viewName, boolean includeSchema,
                                   boolean includeColumns, boolean checkViewExists, boolean checkAuthorization) {
        if (checkAuthorization) databaseAuthorizationService.checkReadPermission(schemaName, viewName);
        return metadataProviderService.getView(schemaName, viewName, includeSchema, includeColumns, checkViewExists);
    }

    @Override
    public List<ViewMetadataDto> getViewsBySchema(String schemaName, boolean includeSchema,
                                                  boolean includeColumns, boolean checkSchemaExists) {
        List<ViewMetadataDto> allViews = metadataProviderService.getViewsBySchema(schemaName, includeSchema, includeColumns, checkSchemaExists);

        // Filter views based on read permissions
        return allViews.stream()
                .filter(view -> AuthorizationUtils.hasPermission(
                        PermissionType.READ,
                        schemaName != null ? schemaName.trim().toLowerCase() : null,
                        view.getViewName() != null ? view.getViewName().trim().toLowerCase() : null))
                .collect(Collectors.toList());
    }

    @Override
    public ViewMetadataDto renameView(UpdateViewDto updateViewDto) {
        databaseAuthorizationService.checkWritePermission(updateViewDto.getSchemaName(), updateViewDto.getViewName());

        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(updateViewDto.getSchemaName());

        if (!updateViewDto.getViewName().equalsIgnoreCase(updateViewDto.getUpdatedViewName())) {
            String renameSql = String.format("RENAME TABLE %s.%s TO %s.%s",
                    validatedSchemaName, SqlSecurityUtils.validateTableName(updateViewDto.getViewName()),
                    validatedSchemaName, SqlSecurityUtils.validateTableName(updateViewDto.getUpdatedViewName()));
            jdbcTemplate.execute(renameSql);
        }

        return getView(updateViewDto.getSchemaName(), updateViewDto.getUpdatedViewName(),
                true, true, false);
    }

    @Override
    public Boolean deleteView(String schemaName, String viewName) {
        // Check delete permission
        databaseAuthorizationService.checkDeletePermission(schemaName, viewName);

        String validatedSchemaName = SqlSecurityUtils.validateSchemaName(schemaName);
        String validatedViewName = SqlSecurityUtils.validateTableName(viewName);

        if (metadataProviderService.isSystemSchemaByName(schemaName)) {
            throw new UnauthorizedActionException("Cannot delete system view: " + schemaName + "." + viewName);
        }

        if (!viewExists(validatedSchemaName, validatedViewName)) {
            throw new ViewNotFoundException(validatedSchemaName.toLowerCase(), validatedViewName.toLowerCase());
        }

        String sql = "DROP VIEW " + validatedSchemaName + "." + validatedViewName;

        jdbcTemplate.execute(sql);
        return !viewExists(validatedSchemaName, validatedViewName);
    }
}
