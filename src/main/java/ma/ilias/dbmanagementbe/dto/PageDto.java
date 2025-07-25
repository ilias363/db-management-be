package ma.ilias.dbmanagementbe.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.Collections;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@SuperBuilder
public class PageDto<T> {
    @Builder.Default
    private List<T> items = Collections.emptyList();
    private long totalItems;
    private int currentPage;
    private int pageSize;
    private int totalPages;
}
