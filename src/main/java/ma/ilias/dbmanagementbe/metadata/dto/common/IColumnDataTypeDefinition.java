package ma.ilias.dbmanagementbe.metadata.dto.common;

public interface IColumnDataTypeDefinition {
    String getDataType();

    Integer getCharacterMaxLength();

    Integer getNumericPrecision();

    Integer getNumericScale();
}
