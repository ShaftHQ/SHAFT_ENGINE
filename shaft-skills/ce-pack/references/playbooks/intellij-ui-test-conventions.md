# IntelliJ UI test conventions

## Icon-only controls (#6013)

`ShaftIconButtons` and other icon-only toolbar controls expose their contract through
**accessible name**, not visible text. Prefer:

```java
findByAccessibleName(panel, "Start recording", JButton.class);
assertEquals("Start recording", button.getAccessibleContext().getAccessibleName());
```

Do **not** assert `getText()` on icon-only Automation/Design toolbar buttons — text is
empty or decorative and flakes across LAFs.

## Automation workflow persistence key (#6014)

The Automation canvas persistence key is **`Live record`**
(`AutomationStagePanel.LIVE_RECORD_TAB`). A legacy **`Guided`** key restores once to
Live record and must not be written again.
