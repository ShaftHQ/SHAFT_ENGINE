# SHAFT Upgrader

`shaft-upgrader` is the standalone, transactional migration module for existing
Maven test automation projects.

It supports:

- Native Selenium, Appium, and REST Assured projects using TestNG or JUnit.
- Legacy `io.github.shafthq:SHAFT_ENGINE` projects.
- Existing modular SHAFT projects.

Run the Python standard-library script from the project being upgraded:

```bash
python3 upgrade_to_modular_shaft.py --project .
```

Use `--dry-run` to preview POM changes. The normal workflow compiles before and
after migration and restores every changed file when the upgraded project does
not compile. Setting `OPENAI_API_KEY` or using `--prompt-for-openai-key` enables
up to three constrained compile-repair attempts before rollback.

See the complete
[upgrade, optional-module detection, AI safety, and rollback documentation](../docs/UPGRADING_TO_MODULAR_SHAFT.md).
