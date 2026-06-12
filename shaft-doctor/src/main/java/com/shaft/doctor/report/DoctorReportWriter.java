package com.shaft.doctor.report;

import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.Finding;
import com.shaft.doctor.model.Remediation;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Collectors;

/**
 * Writes deterministic human-readable Doctor reports.
 */
public final class DoctorReportWriter {
    /**
     * Writes a Markdown report.
     *
     * @param path destination
     * @param bundle evidence bundle
     * @param diagnosis diagnosis
     */
    public void writeMarkdown(Path path, EvidenceBundle bundle, Diagnosis diagnosis) {
        StringBuilder report = new StringBuilder();
        report.append("# SHAFT Doctor Report\n\n");
        report.append("- Evidence schema: `").append(bundle.schemaVersion()).append("`\n");
        report.append("- Diagnosis schema: `").append(diagnosis.schemaVersion()).append("`\n");
        report.append("- Bundle: `").append(bundle.bundleId()).append("`\n");
        report.append("- Primary cause: `").append(diagnosis.primaryCause()).append("`\n");
        report.append("- Confidence: `").append(diagnosis.confidence()).append("`\n");
        report.append("- Evidence items: ").append(bundle.evidence().size()).append("\n");
        report.append("- Omitted by policy or limits: ").append(bundle.redaction().omittedItems()).append("\n\n");

        report.append("## Diagnosis\n\n");
        report.append(diagnosis.summary()).append("\n\n");
        report.append(diagnosis.rationale()).append("\n\n");
        if (!diagnosis.contributingCauses().isEmpty()) {
            report.append("Contributing causes: ")
                    .append(diagnosis.contributingCauses().stream()
                            .map(value -> "`" + value + "`")
                            .collect(Collectors.joining(", ")))
                    .append("\n\n");
        }

        report.append("## Findings\n\n");
        if (diagnosis.findings().isEmpty()) {
            report.append("No findings were emitted.\n\n");
        } else {
            report.append("| Type | Severity | Category | Finding | Evidence |\n");
            report.append("|---|---|---|---|---|\n");
            for (Finding finding : diagnosis.findings()) {
                report.append("| ").append(finding.kind())
                        .append(" | ").append(finding.severity())
                        .append(" | ").append(finding.category())
                        .append(" | ").append(cell(finding.title() + ": " + finding.detail()))
                        .append(" | ").append(finding.evidenceIds().stream()
                                .map(id -> "`" + id + "`")
                                .collect(Collectors.joining(", ")))
                        .append(" |\n");
            }
            report.append("\n");
        }

        report.append("## Remediation\n\n");
        if (diagnosis.remediations().isEmpty()) {
            report.append("No deterministic remediation is supported by the current evidence.\n\n");
        } else {
            for (Remediation remediation : diagnosis.remediations()) {
                report.append("1. **").append(remediation.title()).append("**: ")
                        .append(remediation.action()).append("\n");
            }
            report.append("\n");
        }

        report.append("## Missing Evidence\n\n");
        if (diagnosis.missingEvidence().isEmpty()) {
            report.append("No additional evidence requirement was identified.\n\n");
        } else {
            for (String missing : diagnosis.missingEvidence()) {
                report.append("- ").append(missing).append("\n");
            }
            report.append("\n");
        }

        report.append("## Evidence Index\n\n");
        report.append("| ID | Category | Source | Bytes | Redacted | Truncated | Checksum |\n");
        report.append("|---|---|---|---:|---|---|---|\n");
        bundle.evidence().forEach(item -> report.append("| `").append(item.id())
                .append("` | ").append(item.category())
                .append(" | `").append(cell(item.provenance().sourceReference()))
                .append("` | ").append(item.sizeBytes())
                .append(" | ").append(item.redacted())
                .append(" | ").append(item.truncated())
                .append(" | `").append(item.sha256()).append("` |\n"));
        report.append("\n");

        report.append("## Privacy\n\n");
        report.append("Applied redaction rules: ")
                .append(bundle.redaction().appliedRules().isEmpty()
                        ? "none"
                        : String.join(", ", bundle.redaction().appliedRules()))
                .append(". Sensitive field names removed: ")
                .append(bundle.redaction().removedFieldNames().isEmpty()
                        ? "none"
                        : String.join(", ", bundle.redaction().removedFieldNames()))
                .append(". Screenshot and page-snapshot evidence is retained only when explicitly enabled.\n");

        try {
            Files.createDirectories(path.toAbsolutePath().normalize().getParent());
            Files.writeString(path, report.toString(), StandardCharsets.UTF_8);
        } catch (IOException exception) {
            throw new IllegalStateException("Doctor Markdown report could not be written.", exception);
        }
    }

    private static String cell(String value) {
        return value.replace("|", "\\|").replaceAll("\\s+", " ").trim();
    }
}
