package com.shaft.doctor.report;

import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.DoctorAdvisory;
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
        writeMarkdown(path, bundle, diagnosis, null);
    }

    /**
     * Writes a Markdown report with a separately identified optional advisory.
     *
     * @param path destination
     * @param bundle evidence bundle
     * @param diagnosis deterministic diagnosis
     * @param advisory optional provider advisory
     */
    public void writeMarkdown(
            Path path,
            EvidenceBundle bundle,
            Diagnosis diagnosis,
            DoctorAdvisory advisory) {
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

        if (advisory != null && advisory.status() != DoctorAdvisory.Status.DISABLED) {
            appendAdvisory(report, advisory);
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

    private static void appendAdvisory(StringBuilder report, DoctorAdvisory advisory) {
        report.append("## AI Advisory\n\n");
        report.append("> Advisory only. The deterministic diagnosis above remains authoritative and unchanged.\n\n");
        report.append("- Status: `").append(advisory.status()).append("`\n");
        report.append("- Provider status: `").append(advisory.metadata().providerStatus()).append("`\n");
        report.append("- Provider: `").append(code(advisory.metadata().provider())).append("`\n");
        report.append("- Model: `").append(code(advisory.metadata().model())).append("`\n");
        report.append("- Configuration: `")
                .append(code(advisory.metadata().configurationIdentifier())).append("`\n");
        report.append("- Duration: ").append(advisory.metadata().durationMillis()).append(" ms\n");
        report.append("- Usage: ").append(advisory.metadata().usage().inputTokens())
                .append(" input / ").append(advisory.metadata().usage().outputTokens())
                .append(" output tokens\n");
        report.append("- Cache hit: ").append(advisory.metadata().cacheHit()).append("\n\n");

        if (!advisory.metadata().fallbackReason().isBlank()) {
            report.append("Fallback: ").append(markdown(advisory.metadata().fallbackReason())).append("\n\n");
        }
        if (!advisory.metadata().warnings().isEmpty()) {
            report.append("Warnings:\n\n");
            advisory.metadata().warnings().forEach(warning ->
                    report.append("- ").append(markdown(warning)).append("\n"));
            report.append("\n");
        }
        if (advisory.status() != DoctorAdvisory.Status.SUCCESS) {
            report.append("No provider analysis was accepted. The deterministic report is complete and retained.\n\n");
            return;
        }

        report.append("### Observations\n\n");
        if (advisory.analysis().observations().isEmpty()) {
            report.append("No provider observations were returned.\n\n");
        } else {
            for (DoctorAdvisory.Observation observation : advisory.analysis().observations()) {
                report.append("- ").append(markdown(observation.statement()))
                        .append(" Evidence: ").append(references(observation.evidenceIds()))
                        .append(observation.cited() ? "" : " (uncited)")
                        .append("\n");
            }
            report.append("\n");
        }

        report.append("### Hypotheses\n\n");
        if (advisory.analysis().hypotheses().isEmpty()) {
            report.append("No provider hypotheses were returned.\n\n");
        } else {
            for (DoctorAdvisory.Hypothesis hypothesis : advisory.analysis().hypotheses()) {
                report.append("- `").append(hypothesis.causeCategory()).append("` ")
                        .append("(`").append(hypothesis.confidence()).append("`): ")
                        .append(markdown(hypothesis.statement()))
                        .append(" Evidence: ").append(references(hypothesis.evidenceIds()))
                        .append(hypothesis.cited() ? "" : " (uncited)")
                        .append(hypothesis.contradictsDeterministic()
                                ? " (contradicts deterministic primary cause)" : "")
                        .append("\n");
            }
            report.append("\n");
        }

        report.append("### Recommended Actions\n\n");
        if (advisory.analysis().recommendedActions().isEmpty()) {
            report.append("No provider actions were returned.\n\n");
        } else {
            for (DoctorAdvisory.RecommendedAction action : advisory.analysis().recommendedActions()) {
                report.append("1. **").append(markdown(action.title())).append("**: ")
                        .append(markdown(action.action()))
                        .append(" Evidence: ").append(references(action.evidenceIds()))
                        .append(action.cited() ? "" : " (uncited)")
                        .append("\n");
            }
            report.append("\n");
        }

        report.append("### Provider Missing Evidence\n\n");
        appendTextList(report, advisory.analysis().missingEvidence(),
                "The provider did not identify additional missing evidence.");
        report.append("### Limitations\n\n");
        appendTextList(report, advisory.analysis().limitations(),
                "The provider did not state additional limitations.");
    }

    private static void appendTextList(StringBuilder report, java.util.List<String> values, String emptyText) {
        if (values.isEmpty()) {
            report.append(emptyText).append("\n\n");
            return;
        }
        values.forEach(value -> report.append("- ").append(markdown(value)).append("\n"));
        report.append("\n");
    }

    private static String references(java.util.List<String> evidenceIds) {
        return evidenceIds.isEmpty()
                ? "none"
                : evidenceIds.stream().map(id -> "`" + id + "`").collect(Collectors.joining(", "));
    }

    private static String markdown(String value) {
        return cell(value)
                .replace("\\", "\\\\")
                .replace("`", "\\`")
                .replace("*", "\\*")
                .replace("_", "\\_")
                .replace("[", "\\[")
                .replace("]", "\\]")
                .replace("<", "\\<")
                .replace(">", "\\>");
    }

    private static String code(String value) {
        return cell(value).replace("`", "\\`");
    }

    private static String cell(String value) {
        return value.replace("|", "\\|").replaceAll("\\s+", " ").trim();
    }
}
