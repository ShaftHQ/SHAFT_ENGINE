package com.shaft.tools.internal.support;

import java.text.SimpleDateFormat;
import java.util.Date;

public class PerformanceReportHTMLHelper {
    // Add the SHAFT logo URL as a constant
    private static final String SHAFT_LOGO_URL = "https://github.com/ShaftHQ/SHAFT_ENGINE/raw/main/src/main/resources/images/shaft.png";

    // Method to generate the final HTML report with responsive layout and pagination
    public static String buildHtml(String startTime, String endTime, String executionTime, String performanceTableRows) {
        StringBuilder html = new StringBuilder();

        // Get the current time as the report creation time
        String reportCreationTime = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date());

        // Build the HTML report
        html.append("<html><head><title>Performance Report</title>");

        // Enhanced CSS for layout, responsiveness, and pagination styling
        html.append("<style>")
                .append("body { font-family: Arial, sans-serif; margin: 20px; }")
                .append("h1 { font-size: 24px; text-align: center; margin-bottom: 10px; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }")
                .append("table { width: 100%; border-collapse: collapse; margin-bottom: 20px; font-size: 14px; }")
                .append("th, td { border: 1px solid #ddd; padding: 12px 15px; text-align: center; }")
                .append("th { background-color: #007BFF; color: white; text-transform: uppercase; letter-spacing: 1px; }")
                .append("tr:nth-child(even) { background-color: #f9f9f9; }")
                .append("tr:hover { background-color: #f1f1f1; cursor: pointer; }")
                .append(".pagination { text-align: center; margin-top: 20px; font-size: 14px; }")
                .append(".page-item { display: inline-block; padding: 8px 16px; cursor: pointer; margin: 0 4px; border: 1px solid #ddd; border-radius: 4px; }")
                .append(".page-item:hover { background-color: #ddd; }")
                .append(".header { display: flex; align-items: center; justify-content: center; margin-bottom: 20px; }")  // Logo and title alignment
                .append(".header img { margin-right: 20px; }")  // Space between logo and title
                .append("p { font-size: 14px; margin-bottom: 8px; }")

                // Responsive adjustments with media queries
                .append("@media screen and (max-width: 768px) {")
                .append("  body { margin: 10px; }")
                .append("  h1 { font-size: 20px; }")
                .append("  table { font-size: 12px; }")
                .append("  th, td { padding: 8px 10px; }")
                .append("  .header { flex-direction: column; }")  // Stack logo and title vertically
                .append("  .header img { margin-bottom: 10px; }")
                .append("  .pagination { font-size: 12px; }")
                .append("}")

                // Allow horizontal scrolling for smaller screens
                .append("@media screen and (max-width: 576px) {")
                .append("  table { display: block; overflow-x: auto; white-space: nowrap; }")  // Allow horizontal scroll
                .append("}")
                .append("</style>");

        // JavaScript for pagination
        html.append("<script>")
                .append("var currentPage = 1;")
                .append("var rowsPerPage = 10;")  // Set the number of rows per page to 10

                .append("function changePage(page) {")
                .append("var table = document.getElementById('performanceTable');")
                .append("var rows = table.getElementsByTagName('tr');")
                .append("var totalRows = rows.length - 1;")  // Exclude header row

                .append("if (page < 1) page = 1;")
                .append("if (page > Math.ceil(totalRows / rowsPerPage)) page = Math.ceil(totalRows / rowsPerPage);")

                .append("for (var i = 1; i <= totalRows; i++) {")  // Skip header row (index 0)
                .append("rows[i].style.display = (i > (page - 1) * rowsPerPage && i <= page * rowsPerPage) ? '' : 'none';")
                .append("}")

                .append("currentPage = page;")
                .append("showPageNumbers();")
                .append("}")

                .append("function showPageNumbers() {")
                .append("var pagination = document.getElementById('pagination');")
                .append("pagination.innerHTML = '';")
                .append("var totalRows = document.getElementById('performanceTable').getElementsByTagName('tr').length - 1;")  // Exclude header row
                .append("var totalPages = Math.ceil(totalRows / rowsPerPage);")

                .append("for (var i = 1; i <= totalPages; i++) {")
                .append("pagination.innerHTML += '<span class=\"page-item\" onclick=\"changePage(' + i + ')\">' + i + '</span>';")
                .append("}")
                .append("}")

                .append("window.onload = function() { changePage(1); };")  // Initialize pagination on load
                .append("</script>");

        // HTML body content with logo and title aligned side-by-side
        html.append("</head><body>");

        // Center the logo and title in one line using flexbox
        html.append("<div class='header'>");
        html.append("<img src='").append(SHAFT_LOGO_URL).append("' alt='SHAFT Logo' width='100' height='100'>");
        html.append("<h1>Performance Report</h1>");
        html.append("</div>");

        // Report content (creation time, start time, end time, and execution time)
        html.append("<p><strong>Report Creation Time:</strong> ").append(reportCreationTime).append("</p>");
        html.append("<p><strong>Start Time:</strong> ").append(startTime).append("</p>");
        html.append("<p><strong>End Time:</strong> ").append(endTime).append("</p>");
        html.append("<p><strong>Total Execution Time:</strong> ").append(executionTime).append("</p>");

        // Filter input for the table (optional)
        html.append("<input type='text' id='filterInput' onkeyup='filterTable()' placeholder='Filter by endpoint...' style='margin-bottom: 15px;'>");

        // Start of the performance table with pagination
        html.append("<table id='performanceTable'><thead>");
        html.append("<tr><th>Endpoint</th><th>Requests</th><th>Max Response Time (ms)</th><th>Min Response Time (ms)</th><th>Average Response Time (ms)</th></tr>");
        html.append("</thead><tbody>");

        // Insert the performance data rows
        html.append(performanceTableRows);

        // End of the table and HTML document
        html.append("</tbody></table>");

        // Add pagination controls
        html.append("<div class='pagination' id='pagination'></div>");

        html.append("</body></html>");

        // Return the generated HTML content as a string
        return html.toString();
    }
}
