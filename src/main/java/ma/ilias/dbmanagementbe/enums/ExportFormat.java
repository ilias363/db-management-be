package ma.ilias.dbmanagementbe.enums;

public enum ExportFormat {
    CSV, JSON;

    public static ExportFormat fromParam(String param) {
        if (param == null) return CSV;
        try {
            return valueOf(param.toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("Unsupported export format: " + param);
        }
    }
}
