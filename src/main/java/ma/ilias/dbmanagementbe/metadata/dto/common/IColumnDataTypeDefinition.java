package ma.ilias.dbmanagementbe.metadata.dto.common;

public interface IColumnDataTypeDefinition {
    String getDataType();

    Long getCharacterMaxLength();

    Integer getNumericPrecision();

    Integer getNumericScale();
}
