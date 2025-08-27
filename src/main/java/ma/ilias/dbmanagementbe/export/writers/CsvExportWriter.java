package ma.ilias.dbmanagementbe.export.writers;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class CsvExportWriter implements ExportWriter {
    @Override
    public String contentType() {
        return "text/csv";
    }

    @Override
    public String fileExtension() {
        return ".csv";
    }

    @Override
    public void writeRows(Writer writer, Iterator<Map<String, Object>> rows) throws IOException {
        List<String> headers = null;

        // Writing headers
        if (rows.hasNext()) {
            Map<String, Object> firstRow = rows.next();
            headers = firstRow.keySet().stream().toList();
            writer.append(String.join(",", headers)).append('\n');

            for (int i = 0; i < headers.size(); i++) {
                Object v = firstRow.get(headers.get(i));
                writer.append(escape(v));
                if (i < headers.size() - 1) writer.append(',');
            }
            writer.append('\n');
            writer.flush();
        }

        while (rows.hasNext()) {
            Map<String, Object> row = rows.next();

            for (int i = 0; i < headers.size(); i++) {
                Object v = row.get(headers.get(i));
                writer.append(escape(v));
                if (i < headers.size() - 1) writer.append(',');
            }
            writer.append('\n');
            writer.flush();
        }
    }

    private String escape(Object v) {
        if (v == null) return "";
        String s = v.toString();
        if (s.contains(",") || s.contains("\n") || s.contains("\"")) {
            return '"' + s.replace("\"", "\"\"") + '"';
        }
        return s;
    }
}
