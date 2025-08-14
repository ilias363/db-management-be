package ma.ilias.dbmanagementbe.record.utils;

import lombok.Setter;
import ma.ilias.dbmanagementbe.exception.InvalidFilterException;
import ma.ilias.dbmanagementbe.metadata.dto.column.BaseColumnMetadataDto;
import ma.ilias.dbmanagementbe.record.dto.FilterCriteriaDto;
import ma.ilias.dbmanagementbe.record.dto.SortCriteriaDto;
import ma.ilias.dbmanagementbe.util.SqlSecurityUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class SearchQueryBuilder {
    private final String schemaName;
    private final String objectName;
    private final List<String> whereClauses = new ArrayList<>();
    private final List<String> orderByClauses = new ArrayList<>();
    private final List<Object> parameters = new ArrayList<>();
    @Setter
    private boolean distinct = false;
    private Integer limit;
    private Integer offset;

    public SearchQueryBuilder(String schemaName, String objectName) {
        this.schemaName = schemaName;
        this.objectName = objectName;
    }

    public void addFilter(FilterCriteriaDto filter, BaseColumnMetadataDto columnMeta, boolean
            validate) {
        if (columnMeta == null) return;

        if (validate) {
            // Validate filter criteria against column metadata
            try {
                FilterValidationUtils.validateFilter(filter, columnMeta);
            } catch (InvalidFilterException e) {
                throw new InvalidFilterException(
                        String.format("Invalid filter for column '%s': %s", filter.getColumnName(), e.getMessage()));
            }
        }

        String columnName = filter.getColumnName();

        switch (filter.getOperator()) {
            case EQUALS:
                whereClauses.add(columnName + " = ?");
                parameters.add(filter.getValue());
                break;

            case NOT_EQUALS:
                whereClauses.add(columnName + " != ?");
                parameters.add(filter.getValue());
                break;

            case IS_NULL:
                whereClauses.add(columnName + " IS NULL");
                break;

            case IS_NOT_NULL:
                whereClauses.add(columnName + " IS NOT NULL");
                break;

            case GREATER_THAN:
                whereClauses.add(columnName + " > ?");
                parameters.add(filter.getValue());
                break;

            case GREATER_THAN_OR_EQUAL:
                whereClauses.add(columnName + " >= ?");
                parameters.add(filter.getValue());
                break;

            case LESS_THAN:
                whereClauses.add(columnName + " < ?");
                parameters.add(filter.getValue());
                break;

            case LESS_THAN_OR_EQUAL:
                whereClauses.add(columnName + " <= ?");
                parameters.add(filter.getValue());
                break;

            case BETWEEN:
                whereClauses.add(columnName + " BETWEEN ? AND ?");
                parameters.add(filter.getMinValue());
                parameters.add(filter.getMaxValue());
                break;

            case LIKE:
                if (filter.isCaseSensitive()) {
                    whereClauses.add(columnName + " LIKE ?");
                } else {
                    whereClauses.add("LOWER(" + columnName + ") LIKE LOWER(?)");
                }
                parameters.add(filter.getValue());
                break;

            case NOT_LIKE:
                if (filter.isCaseSensitive()) {
                    whereClauses.add(columnName + " NOT LIKE ?");
                } else {
                    whereClauses.add("LOWER(" + columnName + ") NOT LIKE LOWER(?)");
                }
                parameters.add(filter.getValue());
                break;

            case STARTS_WITH:
                if (filter.isCaseSensitive()) {
                    whereClauses.add(columnName + " LIKE ?");
                } else {
                    whereClauses.add("LOWER(" + columnName + ") LIKE LOWER(?)");
                }
                parameters.add(filter.getValue() + "%");
                break;

            case ENDS_WITH:
                if (filter.isCaseSensitive()) {
                    whereClauses.add(columnName + " LIKE ?");
                } else {
                    whereClauses.add("LOWER(" + columnName + ") LIKE LOWER(?)");
                }
                parameters.add("%" + filter.getValue());
                break;

            case CONTAINS:
                if (filter.isCaseSensitive()) {
                    whereClauses.add(columnName + " LIKE ?");
                } else {
                    whereClauses.add("LOWER(" + columnName + ") LIKE LOWER(?)");
                }
                parameters.add("%" + filter.getValue() + "%");
                break;

            case IN:
                if (filter.getValues() != null && !filter.getValues().isEmpty()) {
                    String placeholders = String.join(",", Collections.nCopies(filter.getValues().size(), "?"));
                    whereClauses.add(columnName + " IN (" + placeholders + ")");
                    parameters.addAll(filter.getValues());
                }
                break;

            case NOT_IN:
                if (filter.getValues() != null && !filter.getValues().isEmpty()) {
                    String placeholders = String.join(",", Collections.nCopies(filter.getValues().size(), "?"));
                    whereClauses.add(columnName + " NOT IN (" + placeholders + ")");
                    parameters.addAll(filter.getValues());
                }
                break;
        }
    }

    public void addGlobalSearch(String searchTerm, List<String> textColumns) {
        if (textColumns.isEmpty()) return;

        List<String> globalSearchClauses = new ArrayList<>();
        for (String column : textColumns) {
            String validatedColumn = SqlSecurityUtils.validateColumnName(column);
            globalSearchClauses.add("LOWER(" + validatedColumn + ") LIKE LOWER(?)");
            parameters.add("%" + searchTerm + "%");
        }

        whereClauses.add("(" + String.join(" OR ", globalSearchClauses) + ")");
    }

    public void addSort(SortCriteriaDto sort) {
        String columnName = SqlSecurityUtils.validateColumnName(sort.getColumnName());
        String direction = sort.getDirection().toUpperCase();
        orderByClauses.add(columnName + " " + direction);
    }

    public void addPagination(int page, int size) {
        this.limit = size;
        this.offset = page * size;
    }

    public String buildSelectQuery() {
        StringBuilder query = new StringBuilder();

        query.append("SELECT ");
        if (distinct) {
            query.append("DISTINCT ");
        }
        query.append("* FROM ").append(schemaName).append(".").append(objectName);

        if (!whereClauses.isEmpty()) {
            query.append(" WHERE ").append(String.join(" AND ", whereClauses));
        }

        if (!orderByClauses.isEmpty()) {
            query.append(" ORDER BY ").append(String.join(", ", orderByClauses));
        }

        if (limit != null) {
            query.append(" LIMIT ").append(limit);
            if (offset != null && offset > 0) {
                query.append(" OFFSET ").append(offset);
            }
        }

        return query.toString();
    }

    public String buildCountQuery() {
        StringBuilder query = new StringBuilder();

        query.append("SELECT COUNT(");
        if (distinct) {
            query.append("DISTINCT *");
        } else {
            query.append("*");
        }
        query.append(") FROM ").append(schemaName).append(".").append(objectName);

        if (!whereClauses.isEmpty()) {
            query.append(" WHERE ").append(String.join(" AND ", whereClauses));
        }

        return query.toString();
    }

    public List<Object> getParameters() {
        return new ArrayList<>(parameters);
    }

    public static List<String> getValidationErrors(List<FilterCriteriaDto> filters,
                                                   Map<String, BaseColumnMetadataDto> columnMetadataMap) {
        List<String> errors = new ArrayList<>();

        if (filters == null || columnMetadataMap == null) {
            return errors;
        }

        for (FilterCriteriaDto filter : filters) {
            try {
                BaseColumnMetadataDto columnMeta = columnMetadataMap.get(filter.getColumnName().trim());
                if (columnMeta == null) {
                    errors.add(String.format("Column '%s' not found in table/view metadata", filter.getColumnName()));
                    continue;
                }

                FilterValidationUtils.validateFilter(filter, columnMeta);
            } catch (InvalidFilterException e) {
                errors.add(String.format("Invalid filter for column '%s': %s", filter.getColumnName(), e.getMessage()));
            }
        }

        return errors;
    }
}
