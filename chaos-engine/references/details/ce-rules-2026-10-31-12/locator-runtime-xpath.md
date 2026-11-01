# L87 LOCATOR-RUNTIME-XPATH

Locator inventory must resolve runtime-built XPath templates (`concat` /
`quote(param)`) from source before classifying a locator as broken.

Enforced by [`lessons_l82_l92.py`](../../../rules/lessons_l82_l92.py) (`evaluate_locator_inventory`) with proven-red unit tests.
