# Parent work-machine Shell playbook (#6051)

## When this applies

Any FreeToken, OpenAI-compat loopback, OpenCode, or local-agency **writer** that
must see the work-machine loopback (`127.0.0.1:1919` / `:8080`) or that
machine's project checkout.

## Do

1. `ListMachines` → confirm the work machine is `connected: true`.
2. Parent `Shell` / `Read` / `AwaitShell` with `machineId=<work-machine-id>` and
   cwd under `$CE_ROG_CHECKOUT` (or the adopter's configured work-machine
   checkout path).
3. Gate before claiming READY:
   `python3 chaos-engine/skills/local-agency/scripts/assert_parent_rog_shell.py`
   `python3 chaos-engine/skills/local-agency/scripts/dispatch.py --prefer freetoken resolve`
   Prefer `--prefer llamacpp` when the locked local stack is the OpenAI-compat
   loopback on `:8080`.
4. Keep long work-machine writers on the **parent** (or parent-driven Shell
   scripts), not a Task executor.

## Do not

- Dispatch a Task with “use machineId …” and assume the child can pass it.
- Probe `:1919` / `:8080` on the box host and call that READY for a
  work-machine wave.
- Set `CE_ALLOW_BOX_LOCAL_AGENCY=1` to silence the gate for product delivery.

## Child Task message if stranded on box

```text
HARD_BLOCKER: Task Shell has no machineId
hostname=<box>
Need parent Shell(machineId=<work-machine>) for work-machine FreeToken/OpenCode (#6051).
Locked adopter default writer is **openai-compat / llamacpp** on `127.0.0.1:8080` (`dispatch.py --prefer llamacpp`); FreeToken is optional companion, not the default.
```
