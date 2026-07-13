package com.shaft.tools.internal.support;

import lombok.Getter;

@Getter
@SuppressWarnings("SpellCheckingInspection")
public enum HTMLHelper {
    CHECKPOINT_COUNTER("""
            <!DOCTYPE html>
            <html lang="en">
            <head>
              <meta charset="UTF-8">
              <meta name="viewport" content="width=device-width, initial-scale=1.0">
              <title>SHAFT Overview</title>
              <style>
            """ + ReportHtmlTheme.style() + """
                .donut {
                  width: 180px;
                  height: 180px;
                  border-radius: 50%;
                  background: conic-gradient(var(--shaft-pass) ${CHECKPOINTS_PASSED_DEGREES}deg, var(--shaft-fail) 0);
                  box-shadow: inset 0 0 0 28px var(--shaft-surface);
                }
              </style>
            </head>
            <body>
              <div class="report-shell">
                <header class="report-header">
                  <div class="report-header-inner">
                    <span class="brand-mark">S</span>
                    <div>
                      <h1>SHAFT Overview</h1>
                      <p class="subtitle">Suite checkpoints, validations, and captured traces</p>
                    </div>
                  </div>
                </header>
                <main class="report-main">
                  <section class="panel">
                    <h2>Summary</h2>
                    <div class="metric-grid">
                      <div class="metric-card"><div class="metric-label">Total</div><div class="metric-value">${CHECKPOINTS_TOTAL}</div></div>
                      <div class="metric-card"><div class="metric-label">Passed</div><div class="metric-value">${CHECKPOINTS_PASSED}</div></div>
                      <div class="metric-card"><div class="metric-label">Failed</div><div class="metric-value">${CHECKPOINTS_FAILED}</div></div>
                      <div class="metric-card"><div class="metric-label">Traces captured</div><div class="metric-value">${TRACES_CAPTURED}</div></div>
                      <div class="metric-card">
                        <div class="metric-label">Pass Ratio</div>
                        <div class="donut-wrap">
                          <div class="donut" role="img" aria-label="${CHECKPOINTS_PASSED} of ${CHECKPOINTS_TOTAL} checkpoints passed"></div>
                          <div class="donut-center">${CHECKPOINTS_PASSED_PERCENTAGE_LABEL}%</div>
                        </div>
                        <div class="chart-legend">
                          <span class="legend-item"><span class="legend-swatch pass"></span>Passed: ${CHECKPOINTS_PASSED}</span>
                          <span class="legend-item"><span class="legend-swatch fail"></span>Failed: ${CHECKPOINTS_FAILED}</span>
                        </div>
                      </div>
                    </div>
                  </section>
                  <section class="panel">
                    <h2>Details</h2>
                    <div class="table-wrap">
                      <table>
                        <thead>
                          <tr><th>ID</th><th>Type</th><th>Message</th><th>Status</th></tr>
                        </thead>
                        <tbody>${CHECKPOINTS_DETAILS}</tbody>
                      </table>
                    </div>
                  </section>
                </main>
              </div>
            </body>
            </html>"""),
    CHECKPOINT_DETAILS_FORMAT("<tr><td>%d</td><td>%s</td><td>%s</td><td><span class=\"status-chip %s\">%s</span></td></tr>"),

    EXECUTION_SUMMARY("""
            <!DOCTYPE html>
            <html lang="en">
            <head>
              <meta charset="UTF-8">
              <meta name="viewport" content="width=device-width, initial-scale=1.0">
              <title>Execution Summary Report</title>
              <style>
            """ + ReportHtmlTheme.style() + """
                .donut {
                  width: 180px;
                  height: 180px;
                  border-radius: 50%;
                  background: conic-gradient(
                    var(--shaft-pass) 0% ${CASES_PASSED_PERCENTAGE_PIE}%,
                    var(--shaft-fail) ${CASES_PASSED_PERCENTAGE_PIE}% ${CASES_FAILED_PERCENTAGE_PIE}%,
                    var(--shaft-warn) ${CASES_FAILED_PERCENTAGE_PIE}%);
                  box-shadow: inset 0 0 0 28px var(--shaft-surface);
                }
                .validation-donut {
                  width: 110px;
                  height: 110px;
                  border-radius: 50%;
                  background: conic-gradient(var(--shaft-pass) ${VALIDATION_PASSED_PERCENTAGE_PIE}deg, var(--shaft-fail) 0);
                  box-shadow: inset 0 0 0 18px var(--shaft-surface);
                }
              </style>
            </head>
            <body>
              <div class="report-shell">
                <header class="report-header">
                  <div class="report-header-inner">
                    <span class="brand-mark">S</span>
                    <div>
                      <h1>Execution Summary Report</h1>
                      <p class="subtitle">${DATE} · ${START_TIME} - ${END_TIME} · ${TOTAL_TIME}</p>
                    </div>
                  </div>
                </header>
                <main class="report-main">
                  <section class="panel">
                    <h2>Run Summary</h2>
                    <div class="metric-grid">
                      <div class="metric-card"><div class="metric-label">Total Test Cases</div><div class="metric-value">${CASES_TOTAL}</div></div>
                      <div class="metric-card"><div class="metric-label">Passed</div><div class="metric-value">${CASES_PASSED}</div></div>
                      <div class="metric-card"><div class="metric-label">Failed</div><div class="metric-value">${CASES_FAILED}</div></div>
                      <div class="metric-card"><div class="metric-label">Skipped</div><div class="metric-value">${CASES_SKIPPED}</div></div>
                      <div class="metric-card">
                        <div class="metric-label">Pass Rate</div>
                        <div class="metric-value">${CASES_PASSED_PERCENTAGE}%</div>
                        <div class="donut" role="img" aria-label="${CASES_PASSED} passed, ${CASES_FAILED} failed, and ${CASES_SKIPPED} skipped of ${CASES_TOTAL} test cases"></div>
                        <div class="chart-legend">
                          <span class="legend-item"><span class="legend-swatch pass"></span>Passed: ${CASES_PASSED}</span>
                          <span class="legend-item"><span class="legend-swatch fail"></span>Failed: ${CASES_FAILED}</span>
                          <span class="legend-item"><span class="legend-swatch warn"></span>Skipped: ${CASES_SKIPPED}</span>
                        </div>
                      </div>
                    </div>
                  </section>
                  <section class="panel">
                    <h2>Validations</h2>
                    <div class="metric-grid">
                      <div class="metric-card"><div class="metric-label">Total</div><div class="metric-value">${VALIDATION_TOTAL}</div></div>
                      <div class="metric-card"><div class="metric-label">Passed</div><div class="metric-value">${VALIDATION_PASSED}</div></div>
                      <div class="metric-card"><div class="metric-label">Failed</div><div class="metric-value">${VALIDATION_FAILED}</div></div>
                      <div class="metric-card">
                        <div class="metric-label">Pass Rate</div>
                        <div class="metric-value">${VALIDATION_PASSED_PERCENTAGE}%</div>
                        <div class="validation-donut" role="img" aria-label="${VALIDATION_PASSED} of ${VALIDATION_TOTAL} validations passed"></div>
                        <div class="chart-legend">
                          <span class="legend-item"><span class="legend-swatch pass"></span>Passed: ${VALIDATION_PASSED}</span>
                          <span class="legend-item"><span class="legend-swatch fail"></span>Failed: ${VALIDATION_FAILED}</span>
                        </div>
                      </div>
                    </div>
                  </section>
                  <section class="panel">
                    <h2>Issue Signals</h2>
                    <div class="metric-grid">
                      <div class="metric-card"><div class="metric-label">Total Issues</div><div class="metric-value">${TOTAL_ISSUES}</div></div>
                      <div class="metric-card"><div class="metric-label">Failed · No Known Issue</div><div class="metric-value">${NO_OPEN_ISSUES_FAILED}</div></div>
                      <div class="metric-card"><div class="metric-label">Passed · Issue Still Open</div><div class="metric-value">${OPEN_ISSUES_PASSED}</div></div>
                      <div class="metric-card"><div class="metric-label">Failed · Known Issue</div><div class="metric-value">${OPEN_ISSUES_FAILED}</div></div>
                    </div>
                  </section>
                  <section class="panel">
                    <h2>Test Case Details</h2>
                    <div class="toolbar">
                      <input id="search" type="search" placeholder="Search test cases">
                      <select id="searchDropDown" aria-label="Filter by status">
                        <option value="">All statuses</option>
                        <option value="${PASSED_DROPDOWN_OPTION}">${PASSED_DROPDOWN_OPTION}</option>
                        <option value="${FAILED_DROPDOWN_OPTION}">${FAILED_DROPDOWN_OPTION}</option>
                        <option value="${SKIPPED_DROPDOWN_OPTION}">${SKIPPED_DROPDOWN_OPTION}</option>
                      </select>
                    </div>
                    <div class="table-wrap">
                      <table>
                        <thead>
                          <tr><th></th><th>ID</th><th>Suite</th><th>Name</th><th>Error</th><th>Status</th><th>Issue ID</th></tr>
                        </thead>
                        <tbody id="table">${CASES_DETAILS}</tbody>
                      </table>
                    </div>
                    <p class="muted">Visit <a href="https://shafthq.github.io/" target="_blank">SHAFT's user guide</a> to learn more about the engine and its capabilities.</p>
                  </section>
                </main>
              </div>
              <script>
                function filterCases() {
                  const text = document.getElementById('search').value.toLowerCase();
                  const status = document.getElementById('searchDropDown').value;
                  document.querySelectorAll('#table tr').forEach(row => {
                    const rowText = row.textContent.toLowerCase();
                    row.style.display = rowText.includes(text) && (!status || row.textContent.includes(status)) ? '' : 'none';
                  });
                }
                document.getElementById('search').addEventListener('input', filterCases);
                document.getElementById('searchDropDown').addEventListener('change', filterCases);
              </script>
            </body>
            </html>
            """),
    EXECUTION_SUMMARY_DETAILS_FORMAT("<tr><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td><span class=\"status-chip %s\">%s</span></td><td>%s</td></tr>");

    private final String value;

    HTMLHelper(String type) {
        this.value = type;
    }
}
