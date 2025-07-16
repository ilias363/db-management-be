package ma.ilias.dbmanagementbe.metadata.dto.common;

/**
 * An interface representing the definition of a database column's data type.
 * DTOs that allow for the definition or modification of a column type should implement this
 * to allow for common validation logic.
 */
public interface ColumnDataTypeDefinition {
    String getDataType();

    Integer getCharacterMaxLength();

    Integer getNumericPrecision();

    Integer getNumericScale();
}
