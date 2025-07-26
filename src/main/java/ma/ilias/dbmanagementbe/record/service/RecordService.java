package ma.ilias.dbmanagementbe.record.service;

import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.record.dto.*;

import java.util.List;
import java.util.Map;

public interface RecordService {
    RecordPageDto getRecords(String schemaName, String tableName, int page, int size,
                             String sortBy, String sortDirection);

    RecordDto getRecord(String schemaName, String tableName, Map<String, Object> primaryKeyValues);

    RecordDto getRecord(String schemaName, String tableName, Map<String, Object> primaryKeyValues,
                        boolean checkAuthorization);

    RecordDto createRecord(NewRecordDto newRecordDto);

    RecordDto updateRecord(UpdateRecordDto updateRecordDto);

    boolean deleteRecord(String schemaName, String tableName, Map<String, Object> primaryKeyValues);

    RecordDto getRecordByValues(String schemaName, String tableName, Map<String, Object> identifyingValues);

    RecordDto getRecordByValues(String schemaName, String tableName, Map<String, Object> identifyingValues,
                                boolean checkAuthorization);

    RecordPageDto getRecordsByValues(String schemaName, String tableName, Map<String, Object> identifyingValues,
                                     int page, int size, String sortBy, String sortDirection);

    List<RecordDto> updateRecordByValues(UpdateRecordByValuesDto updateDto);

    int deleteRecordByValues(DeleteRecordByValuesDto deleteDto);

    List<RecordDto> createRecords(BatchNewRecordsDto batchNewRecords);

    List<RecordDto> updateRecords(BatchUpdateRecordsDto batchUpdateRecords);

    int deleteRecords(BatchDeleteRecordsDto batchDeleteRecords);

    List<RecordDto> updateRecordsByValues(BatchUpdateRecordsByValuesDto batchUpdateByValues);

    int deleteRecordsByValues(BatchDeleteRecordsByValuesDto batchDeleteByValues);

    long getRecordCount(String schemaName, String tableName, boolean checkTableExists);

    long getRecordCount(String schemaName, String tableName, boolean checkTableExists, boolean checkAuthorization);

    void validateRecordData(String schemaName, String tableName, Map<String, Object> data,
                            List<BaseColumnMetadataDto> columns, boolean isUpdate);

    AdvancedSearchResponseDto advancedSearch(AdvancedSearchRequestDto searchRequest);
}
