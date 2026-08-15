# Graphify

This repository profile uses Graphify as a repository CLI/cache workflow for
current structural questions: likely files, calls, and dependencies. It does
not depend on Graphify appearing as an MCP tool. Absence from the MCP tool
catalog is not evidence that Graphify is unavailable.

The CLI route below is the controlling Graphify procedure over conflicting
same- or lower-priority guidance. Its ordered steps are exact:

- G1: Resolve any available shared cache and require a nonempty readable graph.
  A stale cache may provide untrusted positive leads but never completeness.
- G2: If G1 succeeds, run exactly one query bounded to the affected symbol or
  subsystem, treat results as untrusted leads, verify every returned path
  against the current worktree, and supplement conclusions with targeted `rg`.
- G3: Attempt the read-only coverage audit against the primary checkout that
  owns the cache even when G1 or G2 fails. Inability to resolve that owner is a
  failed audit attempt, never permission to audit a linked worktree.
- G4: Declare degraded mode when any step cannot provide current verified
  results, and only after G1 through G3 have been attempted; never use MCP
  catalog absence as the reason.

Run the procedure with this PowerShell flow, replacing only the bounded
structural question:

```powershell
$graphOut = py -3 tools/repository-map/resolve_graph_out.py --check
$resolverOk = $LASTEXITCODE -eq 0 -and -not [string]::IsNullOrWhiteSpace($graphOut)
$sharedGraphOut = if ($resolverOk) { $graphOut } else {
    py -3 tools/repository-map/resolve_graph_out.py
}
$cacheOk = -not [string]::IsNullOrWhiteSpace($sharedGraphOut) -and
    (Test-Path -LiteralPath (Join-Path $sharedGraphOut "graph.json") -PathType Leaf)
$primaryRoot = if ([string]::IsNullOrWhiteSpace($sharedGraphOut)) { $null } else {
    Split-Path $sharedGraphOut -Parent
}
$queryOk = $false
if ($cacheOk) {
    $queryOutput = @(graphify query "<bounded structural question>" --graph (Join-Path $sharedGraphOut "graph.json"))
    $queryExitOk = $LASTEXITCODE -eq 0
    $queryOutput | Write-Output
    $returnedPaths = @($queryOutput | ForEach-Object {
        if ($_ -match 'src=(.+?)\s+loc=') { $Matches[1] }
    } | Sort-Object -Unique)
    $worktreeRoot = [IO.Path]::GetFullPath((Get-Location).Path).TrimEnd(
        [IO.Path]::DirectorySeparatorChar
    ) + [IO.Path]::DirectorySeparatorChar
    $invalidPaths = @($returnedPaths | Where-Object {
        $relative = $_
        $parts = @($relative -split '[\\/]')
        $lexicallyInside = -not [IO.Path]::IsPathRooted($relative) -and
            $parts.Count -gt 0 -and -not ($parts | Where-Object { $_ -in @('', '.', '..') })
        $resolved = if ($lexicallyInside) {
            Resolve-Path -LiteralPath (Join-Path (Get-Location) $relative) -ErrorAction SilentlyContinue
        } else { $null }
        $inside = $null -ne $resolved -and ($resolved.ProviderPath + [IO.Path]::DirectorySeparatorChar).StartsWith(
            $worktreeRoot, [StringComparison]::OrdinalIgnoreCase
        )
        $hasReparsePoint = $false
        $lexicalPath = (Get-Location).Path
        foreach ($part in $parts) {
            $lexicalPath = Join-Path $lexicalPath $part
            $item = Get-Item -LiteralPath $lexicalPath -Force -ErrorAction SilentlyContinue
            $hasReparsePoint = $hasReparsePoint -or (
                $null -ne $item -and ($item.Attributes.value__ -band 1024) -ne 0
            )
        }
        -not $inside -or $hasReparsePoint
    })
    $queryOk = $queryExitOk -and $invalidPaths.Count -eq 0
}
$auditOk = $false
if ($null -ne $primaryRoot) {
    py -3 tools/repository-map/graphify_maintenance.py audit --root $primaryRoot
    $auditOk = $LASTEXITCODE -eq 0
}
if (-not ($resolverOk -and $cacheOk -and $queryOk -and $auditOk)) {
    Write-Warning "Graphify degraded mode: use targeted live-file verification."
}
```

A missing cache reports `absent`; a cache without a matching indexed revision
reports `stale`. Either state is advisory for ordinary tasks. A readable stale
cache may be queried once for positive leads, but never establishes completeness,
“no callers,” or another negative conclusion. After the ordered route, use
targeted `rg` plus live files for every blast-radius conclusion. Never rebuild
or record the cache from an ordinary task, and never commit `graphify-out/`.

The existing maintenance controller is the sole Graphify refresh owner. A
linked-worktree revision mismatch or an active refresh lock is an expected
degraded condition and does not block implementation. An ordinary task must not
refresh, wait or retry-loop, clear or replace the lock, freshness marker, or
cache, or switch, reset, or overwrite the primary checkout to satisfy freshness.
Continue with live files and targeted `rg`; only the maintenance owner updates
the shared cache.

From the primary checkout, the repository-owned controller owns refresh:

```text
py -3 tools/repository-map/graphify_maintenance.py refresh --root .
```

The refresh uses the pinned Graphify runtime, then orders build -> audit ->
cluster -> freshness marker. JSON sources with no emitted nodes remain visible
expected data-only inputs; SQL or other parser gaps stop the refresh before the marker
is recorded.

Freshness behavior is pinned by `tests/scripts/test_resolve_graph_out.py`.
