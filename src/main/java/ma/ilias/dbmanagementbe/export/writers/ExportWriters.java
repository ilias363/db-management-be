package ma.ilias.dbmanagementbe.export.writers;

import ma.ilias.dbmanagementbe.enums.ExportFormat;

public class ExportWriters {
    public static ExportWriter forFormat(ExportFormat format) {
        return switch (format) {
            case CSV -> new CsvExportWriter();
            case JSON -> new JsonExportWriter();
        };
    }
}
