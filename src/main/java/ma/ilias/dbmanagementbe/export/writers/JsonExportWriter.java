package ma.ilias.dbmanagementbe.export.writers;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.Map;

public class JsonExportWriter implements ExportWriter {
    @Override
    public String contentType() {
        return "application/json";
    }

    @Override
    public String fileExtension() {
        return ".json";
    }

    @Override
    public void writeRows(Writer writer, Iterator<Map<String, Object>> rows) throws IOException {
        writer.append('[');

        if (rows.hasNext()) {
            Map<String, Object> firstRow = rows.next();
            writer.append('{');
            boolean firstField = true;
            for (Map.Entry<String, Object> e : firstRow.entrySet()) {
                if (!firstField) writer.append(',');
                firstField = false;
                writer.append('"').append(escape(e.getKey())).append('"').append(':').append(toJsonValue(e.getValue()));
            }
            writer.append('}');
            writer.flush();
        }

        while (rows.hasNext()) {
            Map<String, Object> row = rows.next();
            writer.append('{');
            boolean firstField = true;
            for (Map.Entry<String, Object> e : row.entrySet()) {
                if (!firstField) writer.append(',');
                firstField = false;
                writer.append('"').append(escape(e.getKey())).append('"').append(':').append(toJsonValue(e.getValue()));
            }
            writer.append('}');
            writer.flush();
        }
        writer.append(']');
    }

    private String toJsonValue(Object v) {
        if (v == null) return "null";
        if (v instanceof Number || v instanceof Boolean) return v.toString();
        return '"' + escape(v.toString()) + '"';
    }

    private String escape(String s) {
        return s.replace("\\", "\\\\").replace("\"", "\\\"");
    }
}
