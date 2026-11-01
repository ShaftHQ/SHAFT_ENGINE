# L92 CE-VERSION-AFTER-UPGRADE

After every ChaosEngine upgrade/reinstall, verify `.chaos-engine` / source
`VERSION` matches the intended release before claiming success.

Enforced by [`lessons_l82_l92.py`](../../../rules/lessons_l82_l92.py) (`evaluate_version_match`) with proven-red unit tests.
