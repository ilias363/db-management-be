package ma.ilias.dbmanagementbe.enums;

public enum FilterOperator {
    // Equality
    EQUALS,
    NOT_EQUALS,

    // Null checks
    IS_NULL,
    IS_NOT_NULL,

    // Numeric/Date comparisons
    GREATER_THAN,
    GREATER_THAN_OR_EQUAL,
    LESS_THAN,
    LESS_THAN_OR_EQUAL,
    BETWEEN,

    // String operations
    LIKE,
    NOT_LIKE,
    STARTS_WITH,
    ENDS_WITH,
    CONTAINS,

    // List operations
    IN,
    NOT_IN
}