document.addEventListener('DOMContentLoaded', function() {
    const form = document.getElementById('projectForm');
    const includeWorkflowCheckbox = document.getElementById('includeGitHubWorkflow');
    const workflowTriggerGroup = document.getElementById('workflowTriggerGroup');
    const loadingDiv = document.getElementById('loading');
    const errorDiv = document.getElementById('error');
    const errorMessage = document.getElementById('errorMessage');

    // Show/hide workflow trigger based on checkbox
    includeWorkflowCheckbox.addEventListener('change', function() {
        workflowTriggerGroup.style.display = this.checked ? 'block' : 'none';
    });

    // Handle form submission
    form.addEventListener('submit', async function(e) {
        e.preventDefault();
        
        // Hide error if shown
        errorDiv.style.display = 'none';
        
        // Show loading indicator
        loadingDiv.style.display = 'block';
        form.style.display = 'none';

        const formData = {
            groupId: document.getElementById('groupId').value,
            artifactId: document.getElementById('artifactId').value,
            version: document.getElementById('version').value,
            testRunner: document.getElementById('testRunner').value,
            targetPlatform: document.getElementById('targetPlatform').value,
            executionEnvironment: document.getElementById('executionEnvironment').value,
            includeGitHubWorkflow: document.getElementById('includeGitHubWorkflow').checked,
            workflowTrigger: document.getElementById('workflowTrigger').value
        };

        try {
            const response = await fetch('/generate', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(formData)
            });

            if (!response.ok) {
                throw new Error('Failed to generate project');
            }

            const blob = await response.blob();
            const url = window.URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = formData.artifactId + '.zip';
            document.body.appendChild(a);
            a.click();
            window.URL.revokeObjectURL(url);
            document.body.removeChild(a);

            // Hide loading and show form again
            loadingDiv.style.display = 'none';
            form.style.display = 'block';

        } catch (error) {
            console.error('Error:', error);
            loadingDiv.style.display = 'none';
            errorDiv.style.display = 'block';
            errorMessage.textContent = 'An error occurred while generating the project. Please try again.';
            form.style.display = 'block';
        }
    });
});
