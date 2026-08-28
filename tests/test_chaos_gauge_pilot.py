"""Test suite for ChaosGauge pilot implementation."""

import pytest
from unittest.mock import Mock, patch, PropertyMock
from datetime import datetime, timedelta
from typing import Dict, List, Optional
import json

from shaft_engine.chaos_gauge import (
    ChaosGaugePilot,
    CampaignResult,
    TrialResult,
    AuditReport,
    EvidenceBinding,
    Manifest
)
from shaft_engine.exceptions import (
    InfrastructureFailureError,
    RateLimitError,
    ProviderUnavailableError,
    PartialUploadError,
    SecretLeakError
)


@pytest.fixture
def mock_manifest():
    """Create a mock immutable manifest."""
    manifest = Mock(spec=Manifest)
    manifest.revision = "abc123def456"
    manifest.timestamp = datetime.now()
    manifest.tasks = [
        {"id": f"task_{i}", "description": f"Test task {i}"}
        for i in range(16)
    ]
    return manifest


@pytest.fixture
def mock_evidence_binding():
    """Create a mock evidence binding."""
    binding = Mock(spec=EvidenceBinding)
    binding.manifest_revision = "abc123def456"
    binding.trial_id = "trial_001"
    binding.evidence_hash = "evid_hash_001"
    binding.timestamp = datetime.now()
    return binding


@pytest.fixture
def chaos_gauge_pilot(mock_manifest):
    """Create a ChaosGaugePilot instance with mocked dependencies."""
    pilot = ChaosGaugePilot(
        manifest=mock_manifest,
        campaign_id="chaos_gauge_pilot_001",
        target_tasks=16
    )
    return pilot


class TestChaosGaugePilot:
    """Test suite for ChaosGaugePilot core functionality."""

    def test_campaign_initialization(self, chaos_gauge_pilot, mock_manifest):
        """Test that campaign initializes with correct parameters."""
        assert chaos_gauge_pilot.campaign_id == "chaos_gauge_pilot_001"
        assert chaos_gauge_pilot.target_tasks == 16
        assert chaos_gauge_pilot.manifest == mock_manifest
        assert chaos_gauge_pilot.status == "initialized"

    def test_run_all_trials_successfully(self, chaos_gauge_pilot):
        """Test that all 16 trials run and bind to manifest."""
        with patch.object(chaos_gauge_pilot, '_execute_trial') as mock_execute:
            mock_execute.return_value = TrialResult(
                trial_id="trial_001",
                status="completed",
                evidence_hash="evid_hash_001",
                timestamp=datetime.now()
            )
            
            results = chaos_gauge_pilot.run_campaign()
            
            assert len(results.trials) == 16
            assert all(
                trial.manifest_revision == chaos_gauge_pilot.manifest.revision
                for trial in results.trials
            )
            assert chaos_gauge_pilot.status == "completed"

    def test_evidence_binding_to_immutable_manifest(self, chaos_gauge_pilot, mock_evidence_binding):
        """Test that evidence binds to the merged revision."""
        with patch.object(chaos_gauge_pilot, '_bind_evidence') as mock_bind:
            mock_bind.return_value = mock_evidence_binding
            
            binding = chaos_gauge_pilot.bind_evidence(
                trial_id="trial_001",
                evidence_data={"result": "success"}
            )
            
            assert binding.manifest_revision == "abc123def456"
            assert binding.trial_id == "trial_001"
            assert binding.evidence_hash is not None

    def test_audit_completed_results(self, chaos_gauge_pilot):
        """Test auditing of completed results."""
        mock_results = CampaignResult(
            campaign_id="chaos_gauge_pilot_001",
            trials=[
                TrialResult(
                    trial_id=f"trial_{i}",
                    status="completed",
                    evidence_hash=f"evid_hash_{i}",
                    timestamp=datetime.now()
                )
                for i in range(16)
            ]
        )
        
        with patch.object(chaos_gauge_pilot, '_audit_results') as mock_audit:
            mock_audit.return_value = AuditReport(
                campaign_id="chaos_gauge_pilot_001",
                broken_tasks=[],
                reward_hacking_attempts=[],
                refusals=[],
                contamination_checks=[],
                exclusions=[],
                safety_anomalies=[],
                is_clean=True
            )
            
            audit_report = chaos_gauge_pilot.audit_results(mock_results)
            
            assert audit_report.is_clean
            assert len(audit_report.broken_tasks) == 0
            assert len(audit_report.reward_hacking_attempts) == 0

    def test_audit_detects_broken_tasks(self, chaos_gauge_pilot):
        """Test that audit detects broken tasks."""
        mock_results = CampaignResult(
            campaign_id="chaos_gauge_pilot_001",
            trials=[
                TrialResult(
                    trial_id="trial_1",
                    status="failed",
                    error="Infrastructure failure",
                    timestamp=datetime.now()
                )
            ]
        )
        
        with patch.object(chaos_gauge_pilot, '_audit_results') as mock_audit:
            mock_audit.return_value = AuditReport(
                campaign_id="chaos_gauge_pilot_001",
                broken_tasks=["trial_1"],
                reward_hacking_attempts=[],
                refusals=[],
                contamination_checks=[],
                exclusions=[],
                safety_anomalies=[],
                is_clean=False
            )
            
            audit_report = chaos_gauge_pilot.audit_results(mock_results)
            
            assert not audit_report.is_clean
            assert "trial_1" in audit_report.broken_tasks

    def test_audit_detects_reward_hacking(self, chaos_gauge_pilot):
        """Test that audit detects reward hacking attempts."""
        mock_results = CampaignResult(
            campaign_id="chaos_gauge_pilot_001",
            trials=[
                TrialResult(
                    trial_id="trial_2",
                    status="completed",
                    evidence_hash="suspicious_hash",
                    reward_manipulation_score=0.95,
                    timestamp=datetime.now()
                )
            ]
        )
        
        with patch.object(chaos_gauge_pilot, '_audit_results') as mock_audit:
            mock_audit.return_value = AuditReport(
                campaign_id="chaos_gauge_pilot_001",
                broken_tasks=[],
                reward_hacking_attempts=["trial_2"],
                refusals=[],
                contamination_checks=[],
                exclusions=[],
                safety_anomalies=[],
                is_clean=False
            )
            
            audit_report = chaos_gauge_pilot.audit_results(mock_results)
