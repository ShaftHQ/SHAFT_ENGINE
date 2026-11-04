# Browser guidance (infinite feeds)

## Prefer End / Page Down

On LinkedIn feeds, search results, and Activity → Posts lists:

1. Press `End` (or `Page Down`) once.
2. Wait briefly for lazy-loaded content.
3. Extract the newly visible batch.
4. Repeat.

## Avoid

- Long mouse-wheel scroll loops.
- Continuous wheel events against infinite-scroll containers.

Repeated wheel scrolling on heavy LinkedIn DOMs has hung the remote desktop
browser. Keyboard paging keeps the session usable.
