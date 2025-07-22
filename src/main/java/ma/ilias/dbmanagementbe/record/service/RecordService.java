package ma.ilias.dbmanagementbe.record.service;

import ma.ilias.dbmanagementbe.record.dto.RecordPageDto;

public interface RecordService {
    RecordPageDto getRecords(String schemaName, String tableName, int page, int size,
                             String sortBy, String sortDirection);

    long getRecordCount(String schemaName, String tableName, boolean checkTableExists);
}
