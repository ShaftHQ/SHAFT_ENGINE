package com.shaft.tools.internal.support;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

public class PerformanceReportHTMLHelper {
    private static final DateTimeFormatter REPORT_CREATION_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    // Method to generate the final HTML report with responsive layout and pagination
    public static String buildHtml(String startTime, String endTime, String executionTime, String performanceTableRows) {
        String reportCreationTime = REPORT_CREATION_FORMATTER.format(ZonedDateTime.now());
        return """
                <!DOCTYPE html>
                <html lang="en">
                <head>
                  <meta charset="UTF-8">
                  <meta name="viewport" content="width=device-width, initial-scale=1">
                  <title>Performance Report</title>
                  <style>
                """ + ReportHtmlTheme.style() + """
                  </style>
                  <script>
                    var currentPage = 1;
                    var rowsPerPage = 10;
                    function visibleRows() {
                      return Array.from(document.querySelectorAll('#performanceTable tbody tr'))
                        .filter(row => row.dataset.filtered !== 'true');
                    }
                    function changePage(page) {
                      var rows = visibleRows();
                      var totalPages = Math.max(1, Math.ceil(rows.length / rowsPerPage));
                      currentPage = Math.min(Math.max(page, 1), totalPages);
                      document.querySelectorAll('#performanceTable tbody tr').forEach(row => row.style.display = 'none');
                      rows.forEach((row, index) => {
                        row.style.display = index >= (currentPage - 1) * rowsPerPage && index < currentPage * rowsPerPage ? '' : 'none';
                      });
                      showPageNumbers(totalPages);
                    }
                    function filterTable() {
                      var value = document.getElementById('filterInput').value.toLowerCase();
                      document.querySelectorAll('#performanceTable tbody tr').forEach(row => {
                        row.dataset.filtered = row.textContent.toLowerCase().includes(value) ? 'false' : 'true';
                      });
                      changePage(1);
                    }
                    function showPageNumbers(totalPages) {
                      var pagination = document.getElementById('pagination');
                      pagination.innerHTML = '';
                      for (var i = 1; i <= totalPages; i++) {
                        var button = document.createElement('button');
                        button.type = 'button';
                        button.className = i === currentPage ? '' : 'secondary';
                        button.textContent = i;
                        button.addEventListener('click', changePage.bind(null, i));
                        pagination.appendChild(button);
                      }
                    }
                    window.addEventListener('load', function() { filterTable(); });
                  </script>
                </head>
                <body>
                  <div class="report-shell">
                    <header class="report-header">
                      <div class="report-header-inner">
                        <span class="brand-mark">S</span>
                        <div>
                          <h1>Performance Report</h1>
                          <p class="subtitle">Created %s · %s - %s · %s</p>
                        </div>
                      </div>
                    </header>
                    <main class="report-main">
                      <section class="panel">
                        <h2>API Endpoint Performance</h2>
                        <div class="toolbar">
                          <input type="search" id="filterInput" oninput="filterTable()" placeholder="Filter by endpoint">
                        </div>
                        <div class="table-wrap">
                          <table id="performanceTable">
                            <thead>
                              <tr><th>Endpoint</th><th>Requests</th><th>Max Response Time (ms)</th><th>Min Response Time (ms)</th><th>Average Response Time (ms)</th><th>P50 Response Time (ms)</th><th>P90 Response Time (ms)</th><th>P95 Response Time (ms)</th><th>P99 Response Time (ms)</th><th>Budget (p95 ms)</th><th>Budget Status</th></tr>
                            </thead>
                            <tbody>
                """.formatted(reportCreationTime, startTime, endTime, executionTime)
                + performanceTableRows
                + """
                            </tbody>
                          </table>
                        </div>
                        <div class="toolbar" id="pagination"></div>
                      </section>
                    </main>
                  </div>
                </body>
                </html>
                """;
    }
}
