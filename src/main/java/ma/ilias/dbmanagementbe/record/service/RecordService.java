package ma.ilias.dbmanagementbe.record.service;

import ma.ilias.dbmanagementbe.record.dto.RecordDto;
import ma.ilias.dbmanagementbe.record.dto.RecordPageDto;

import java.util.Map;

public interface RecordService {
    RecordPageDto getRecords(String schemaName, String tableName, int page, int size,
                             String sortBy, String sortDirection);

    RecordDto getRecord(String schemaName, String tableName, Map<String, Object> primaryKeyValues);

    long getRecordCount(String schemaName, String tableName, boolean checkTableExists);
}
