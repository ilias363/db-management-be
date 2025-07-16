package ma.ilias.dbmanagementbe.metadata.dto;

public interface IReferencedColumnReference {
    String getReferencedSchemaName();

    String getReferencedTableName();

    String getReferencedColumnName();
}
