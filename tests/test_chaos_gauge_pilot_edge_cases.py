"""Test suite for ChaosGauge pilot edge cases."""

import pytest
from unittest.mock import Mock, patch
from datetime import datetime
import time

from shaft_engine.chaos_gauge import ChaosGaugePilot, TrialResult
from shaft_engine.exceptions import (
    InfrastructureFailureError,
    RateLimitError,
    ProviderUnavailableError,
    PartialUploadError,
    SecretLeakError
)


class TestChaosGaugePilotEdgeCases:
    """Test edge cases for ChaosGaugePilot."""

    @pytest.fixture
    def pilot(self):
        """Create a basic pilot instance."""
        manifest = Mock()
        manifest.revision = "test_revision"
        manifest.tasks = [{"id": f"task_{i}"} for i in range(16)]
        
        pilot = ChaosGaugePilot(
            manifest=manifest,
            campaign_id="edge_case_test",
            target_tasks=16
        )
        return pilot

    def test_provider_unavailability(self, pilot):
        """Test handling of provider/model availability changes."""
        with patch.object(pilot, '_execute_trial') as mock_execute:
            mock_execute.side_effect = ProviderUnavailableError(
                "Provider OpenAI is currently unavailable"
            )
            
            with pytest.raises(ProviderUnavailableError):
                pilot.run_campaign()
            
            assert pilot.status == "failed"
            assert "provider_unavailable" in pilot.error_log

    def test_rate_limits(self, pilot):
        """Test handling of rate limit errors."""
        with patch.object(pilot, '_execute_trial') as mock_execute:
            mock_execute.side_effect = RateLimitError(
                "Rate limit exceeded. Retry after 60 seconds"
            )
            
            with pytest.raises(RateLimitError):
                pilot.run_campaign()
            
            assert pilot.status == "rate_limited"
            assert pilot.retry_count > 0

    def test_infrastructure_failures(self, pilot):
        """Test handling of infrastructure failures."""
        with patch.object(pilot, '_execute_trial') as mock_execute:
            mock_execute.side_effect = InfrastructureFailureError(
                "Database connection lost"
            )
            
            with pytest.raises(InfrastructureFailureError):
                pilot.run_campaign()
            
            assert pilot.status == "infrastructure_failure"

    def test_partial_uploads(self, pilot):
        """Test handling of partial uploads."""
        with patch.object(pilot, '_upload_evidence') as mock_upload:
            mock_upload.side_effect = PartialUploadError(
                "Only 50% of evidence uploaded"
            )
            
            with pytest.raises(PartialUploadError):
                pilot.complete_campaign()
            
            assert pilot.upload_progress < 100

    def test_secret_bearing_artifacts(self, pilot):
        """Test detection of secret-bearing artifacts."""
        with patch.object(pilot, '_scan_for_secrets') as mock_scan:
            mock_scan.return_value = ["API_KEY", "PASSWORD"]
            
            with pytest.raises(SecretLeakError):
                pilot.validate_evidence(evidence_data={"api_key": "sk-12345"})
            
            assert pilot.secrets_detected == 2

    def test_statistically_inconclusive_results(self, pilot):
        """Test handling of statistically inconclusive results."""
        with patch.object(pilot, '_analyze_statistical_significance') as mock_analyze:
            mock_analyze.return_value = {
                "is_significant": False,
                "p_value": 0.15,
                "confidence_interval": [-0.5, 1.5]
            }
            
            result = pilot.analyze_results()
            
            assert not result["is_significant"]
            assert result["p_value"] > 0.05
            assert "inconclusive" in result["recommendation"]

    def test_chaos_engine_regression(self, pilot):
        """Test detection of ChaosEngine regression."""
        with patch.object(pilot, '_compare_with_baseline') as mock_compare:
            mock_compare.return_value = {
                "regression_detected": True,
                "regression_magnitude": -0.25,
                "affected_metrics": ["accuracy", "latency"]
            }
            
            result = pilot.detect_regression()
            
            assert result["regression_detected"]
            assert result["regression_magnitude"] < 0
            assert "accuracy" in result["affected_metrics"]

    def test_concurrent_trial_execution(self, pilot):
        """Test concurrent execution of trials."""
        with patch.object(pilot, '_execute_trial_async') as mock_async:
            mock_async.return_value = [
                TrialResult(
                    trial_id=f"trial_{i}",
                    status="completed",
                    timestamp=datetime.now()
                )
                for i in range(16)
            ]
            
            results = pilot.run_campaign_concurrent(max_workers=4)
            
            assert len(results) == 16
            assert all(r.status == "completed" for r in results)

    def test_campaign_recovery(self, pilot):
        """Test campaign recovery after failure."""
        with patch.object(pilot, '_execute_trial') as mock_execute:
            mock_execute.side_effect = [
                InfrastructureFailureError("Temporary failure"),
                TrialResult(trial_id="trial_1", status="completed", timestamp=datetime.now())
            ]
