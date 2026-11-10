"""Confirm a defensive finding only after an independent pass."""

def classify(repo_owned: bool, verifier_is_writer: bool, second_pass_verified: bool) -> str:
    if not repo_owned:
        return "stop"
    if verifier_is_writer or not second_pass_verified:
        return "unconfirmed"
    return "confirmed"
