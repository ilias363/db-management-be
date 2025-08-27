package ma.ilias.dbmanagementbe.util;

import ma.ilias.dbmanagementbe.enums.ActionType;

import java.text.MessageFormat;
import java.util.EnumMap;
import java.util.Map;

public class AuditDescriptionBuilder {

    private static final Map<ActionType, String> DESCRIPTION_TEMPLATES = new EnumMap<>(ActionType.class);

    static {
        DESCRIPTION_TEMPLATES.put(ActionType.LOGIN, "User {0} logged in");
        DESCRIPTION_TEMPLATES.put(ActionType.LOGOUT, "User {0} logged out");

        DESCRIPTION_TEMPLATES.put(ActionType.CREATE_SCHEMA, "Created schema ''{0}''");
        DESCRIPTION_TEMPLATES.put(ActionType.DELETE_SCHEMA, "Deleted schema ''{0}''");

        DESCRIPTION_TEMPLATES.put(ActionType.CREATE_TABLE, "Created table ''{1}'' in schema ''{0}''");
        DESCRIPTION_TEMPLATES.put(ActionType.UPDATE_TABLE, "Updated table ''{1}'' in schema ''{0}''");
        DESCRIPTION_TEMPLATES.put(ActionType.DELETE_TABLE, "Deleted table ''{1}'' from schema ''{0}''");

        DESCRIPTION_TEMPLATES.put(ActionType.CREATE_COLUMN, "Created column ''{2}'' in table ''{0}.{1}''");
        DESCRIPTION_TEMPLATES.put(ActionType.UPDATE_COLUMN, "Updated column ''{2}'' in table ''{0}.{1}''");
        DESCRIPTION_TEMPLATES.put(ActionType.DELETE_COLUMN, "Deleted column ''{2}'' from table ''{0}.{1}''");

        DESCRIPTION_TEMPLATES.put(ActionType.CREATE_INDEX, "Created index ''{2}'' on table ''{0}.{1}''");
        DESCRIPTION_TEMPLATES.put(ActionType.DELETE_INDEX, "Deleted index ''{2}'' from table ''{0}.{1}''");

        DESCRIPTION_TEMPLATES.put(ActionType.CREATE_VIEW, "Created view ''{1}'' in schema ''{0}''");
        DESCRIPTION_TEMPLATES.put(ActionType.UPDATE_VIEW, "Updated view ''{1}'' in schema ''{0}''");
        DESCRIPTION_TEMPLATES.put(ActionType.DELETE_VIEW, "Deleted view ''{1}'' from schema ''{0}''");

        DESCRIPTION_TEMPLATES.put(ActionType.CREATE_RECORD, "Created record in table ''{0}.{1}''");
        DESCRIPTION_TEMPLATES.put(ActionType.UPDATE_RECORD, "Updated record in table ''{0}.{1}''");
        DESCRIPTION_TEMPLATES.put(ActionType.DELETE_RECORD, "Deleted record from table ''{0}.{1}''");

        DESCRIPTION_TEMPLATES.put(ActionType.CREATE_MULTIPLE_RECORDS, "Created multiple records in table ''{0}.{1}''");
        DESCRIPTION_TEMPLATES.put(ActionType.UPDATE_MULTIPLE_RECORDS, "Updated multiple records in table ''{0}.{1}''");
        DESCRIPTION_TEMPLATES.put(ActionType.DELETE_MULTIPLE_RECORDS, "Deleted multiple records from table ''{0}.{1}''");

        DESCRIPTION_TEMPLATES.put(ActionType.CREATE_USER, "Created user ''{0}''");
        DESCRIPTION_TEMPLATES.put(ActionType.UPDATE_USER, "Updated user ''{0}''");
        DESCRIPTION_TEMPLATES.put(ActionType.DELETE_USER, "Deleted user ''{0}''");

        DESCRIPTION_TEMPLATES.put(ActionType.ACTIVATE_USER, "Activated user ''{0}''");
        DESCRIPTION_TEMPLATES.put(ActionType.DEACTIVATE_USER, "Deactivated user ''{0}''");

        DESCRIPTION_TEMPLATES.put(ActionType.CREATE_ROLE, "Created role ''{0}''");
        DESCRIPTION_TEMPLATES.put(ActionType.UPDATE_ROLE, "Updated role ''{0}''");
        DESCRIPTION_TEMPLATES.put(ActionType.DELETE_ROLE, "Deleted role ''{0}''");

        DESCRIPTION_TEMPLATES.put(ActionType.CREATE_PERMISSION, "Created permission ''{0}''");
        DESCRIPTION_TEMPLATES.put(ActionType.UPDATE_PERMISSION, "Updated permission ''{0}''");
        DESCRIPTION_TEMPLATES.put(ActionType.DELETE_PERMISSION, "Deleted permission ''{0}''");

        DESCRIPTION_TEMPLATES.put(ActionType.EXECUTE_SQL, "Executed SQL ''{0}''");

        DESCRIPTION_TEMPLATES.put(ActionType.EXPORT_DATA, "Exported data from ''{0}''");
    }

    public static String build(ActionType actionType, Object... params) {
        String template = DESCRIPTION_TEMPLATES.get(actionType);
        if (template == null) {
            return "Unknown action: " + actionType.name();
        }
        return MessageFormat.format(template, params);
    }
}
