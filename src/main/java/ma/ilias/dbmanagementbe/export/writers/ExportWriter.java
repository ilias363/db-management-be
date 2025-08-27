package ma.ilias.dbmanagementbe.export.writers;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.Map;

public interface ExportWriter {
    String contentType();

    String fileExtension();

    void writeRows(Writer writer, Iterator<Map<String, Object>> rows) throws IOException;
}
