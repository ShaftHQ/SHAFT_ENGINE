# Mobile session policy

One canonical rule for emulator cold-boot, memory pressure, and the one-device Appium session. Specialists link here instead of restating it.

## Cold boot

- Cold-boot a wedged emulator with `-no-snapshot-load` when the log says the snapshot failed to load or a different renderer was configured, and when the memory cap killed qemu and left Appium recorded. A snapshot or renderer wedge, and a memory-cap kill that leaves Appium alive after qemu is gone, retries once with `-no-snapshot-load`.
- Dead, offline, or snapshot mismatch uses a cold boot (`-no-snapshot`), not a recycle between test methods.
- `-no-snapshot-save` is not a cold boot.
- Recycle cadence is pressure-or-wedge: only when the memory cap killed qemu or a snapshot or renderer wedge is detected. Do not recycle per method or between long suites. Do not recycle per test method.
- No host qemu time-versus-memory measurement was available when this cadence was chosen, so a timer was not invented.
- An emulator that is still alive while Appium is gone is not killed.
- Do not cold-boot or wipe between language cohorts when the emulator is healthy. Cold-boot belongs to the pressure-or-wedge cadence when the emulator is wedged.

## Memory pressure

- The 8 GiB cap (#6825) stays. `shaft-cli setup start` on Linux runs the managed Android emulator under a `systemd-run --user` cgroup capped at 8 GiB (`-Dshaft.android.emulator.memoryMaxGb=<n>`, `0` disables).
- Headless qemu host RSS can grow far beyond guest RAM and OOM the desktop. On systemd user systems a manual scope is `systemd-run --user --scope -p MemoryMax=10G -p MemorySwapMax=0` around the emulator start. Non-systemd hosts: document an equivalent cgroup or ulimit and refuse unbounded qemu.
- `mobile_toolchain_status` warns when an emulator host process exceeds `-Dshaft.android.emulator.memoryWarnGb` (default 6) so the agent can plan a cold restart. Managed emulators already start under an 8 GiB memory cap on Linux.

## One device, one Appium

- One device has one Appium session and one Maven run.
- Prefer the SHAFT-managed Appium under `~/.local/share/shafthq/*/tools/appium/<version>` over a random global install. Start once with `--relaxed-security --base-path / --port 4723`. Reuse one process. Confirm `GET /status` before Maven. Do not start a second Appium beside a healthy one this agent owns.
- Stop the Appium this agent started when the suite ends, including after a failure. Do not stop an emulator the user asked to keep.
- The managed lease serializes processes it owns. The same rule applies to an Appium or Maven process the lease did not start: do not start a second one beside it. Do not start a second server beside a leftover the lease did not start.
- Isolate tests inside one Appium session: `terminateApp`, `mobile: clearApp`, `activateApp`. Do not restart the session, the Appium server, or the emulator for isolation. iOS real devices: `removeApp`, `installApp`, `activateApp`; `mobile: clearApp` is simulator-only (#7071).
- Before reusing a session, probe it (page source) and replace a dead one. Teardown tolerates a session that setup never created (#7085).
