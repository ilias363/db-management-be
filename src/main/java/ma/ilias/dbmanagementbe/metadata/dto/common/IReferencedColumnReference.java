package ma.ilias.dbmanagementbe.metadata.dto.common;

public interface IReferencedColumnReference {
    String getReferencedSchemaName();

    String getReferencedTableName();

    String getReferencedColumnName();
}
