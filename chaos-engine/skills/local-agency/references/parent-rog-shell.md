# Parent ROG Shell playbook (#6051)

## When this applies

Any FreeToken, llama.cpp, OpenCode, or local-agency **writer** that must see
ROG loopback (`127.0.0.1:1919` / `:8080`) or the ROG checkout.

## Do

1. `ListMachines` → confirm ROG `connected: true`.
2. Parent `Shell` / `Read` / `AwaitShell` with
   `machineId=<rog-id>` and
   cwd under `/media/mohab/OS/Users/Mohab/IdeaProjects/SHAFT_ENGINE`.
3. Gate before claiming READY:
   `python3 chaos-engine/skills/local-agency/scripts/assert_parent_rog_shell.py`
   `python3 chaos-engine/skills/local-agency/scripts/dispatch.py --prefer freetoken resolve`
4. Keep long ROG writers on the **parent** (or parent-driven Shell scripts),
   not a Task executor.

## Do not

- Dispatch a Task with “use machineId …” and assume the child can pass it.
- Probe `:1919` on the box and call that FreeToken READY for a ROG wave.
- Set `CE_ALLOW_BOX_LOCAL_AGENCY=1` to silence the gate for product delivery.

## Child Task message if stranded on box

```text
HARD_BLOCKER: Task Shell has no machineId
hostname=<box>
Need parent Shell(machineId=<rog>) for ROG FreeToken/OpenCode (#6051).
```
