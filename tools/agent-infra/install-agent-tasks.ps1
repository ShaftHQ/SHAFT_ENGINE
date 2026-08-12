[CmdletBinding(PositionalBinding = $false)]
param(
    [string]$MaintenanceHome = 'D:\ShaftNightly',
    [ValidateSet('', 'valid', 'foreign', 'duplicate', 'wrong-owner', 'unprotected')]
    [string]$AclPredicateSelfTest = '',
    [ValidateSet('', 'baseline', 'controller', 'wrapper')]
    [string]$BundleHashSelfTest = '',
    [string]$SecureDirectorySelfTest = '',
    [string]$BundlePromotionSelfTest = '',
    [string]$InstallerLockSelfTest = '',
    [int]$InstallerLockHoldMilliseconds = 0,
    [string]$InstallerLockFailureSelfTest = ''
)
$ErrorActionPreference = 'Stop'
if ($args.Count -ne 0) { throw "Unknown installer arguments: $args" }

Add-Type -TypeDefinition @'
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace ShaftMaintenance {
    public static class SecureDirectory {
        [StructLayout(LayoutKind.Sequential)]
        private struct SecurityAttributes {
            public int Length;
            public IntPtr SecurityDescriptor;
            [MarshalAs(UnmanagedType.Bool)] public bool InheritHandle;
        }

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        private static extern bool CreateDirectoryW(
            string path, ref SecurityAttributes securityAttributes);

        public static void Create(string path, byte[] securityDescriptor) {
            IntPtr descriptor = Marshal.AllocHGlobal(securityDescriptor.Length);
            try {
                Marshal.Copy(securityDescriptor, 0, descriptor, securityDescriptor.Length);
                var attributes = new SecurityAttributes {
                    Length = Marshal.SizeOf(typeof(SecurityAttributes)),
                    SecurityDescriptor = descriptor,
                    InheritHandle = false
                };
                if (!CreateDirectoryW(path, ref attributes)) {
                    throw new Win32Exception(Marshal.GetLastWin32Error());
                }
            } finally {
                Marshal.FreeHGlobal(descriptor);
            }
        }
    }
}
'@

function Get-ControllerBundleHash([string]$ControllerDigest, [string]$WrapperDigest) {
    $sha256 = [Security.Cryptography.SHA256]::Create()
    try {
        $inputBytes = [Text.Encoding]::ASCII.GetBytes("$ControllerDigest`n$WrapperDigest")
        return ([BitConverter]::ToString($sha256.ComputeHash($inputBytes))).Replace('-', '').Substring(0, 32)
    } finally {
        $sha256.Dispose()
    }
}

function Write-Utf8NoBomJson($Value, [string]$Path) {
    $encoding = New-Object Text.UTF8Encoding($false)
    [IO.File]::WriteAllText($Path, ($Value | ConvertTo-Json), $encoding)
}

if ($BundleHashSelfTest) {
    $controllerDigest = 'A' * 64
    $wrapperDigest = 'B' * 64
    if ($BundleHashSelfTest -eq 'controller') { $controllerDigest = ('A' * 63) + 'C' }
    if ($BundleHashSelfTest -eq 'wrapper') { $wrapperDigest = ('B' * 63) + 'D' }
    Get-ControllerBundleHash $controllerDigest $wrapperDigest
    exit 0
}

function Test-ExclusiveRuleSet($OwnerSid, $ExpectedOwnerSid, $Rules, $AllowedSids, $IsProtected) {
    if (-not $IsProtected -or $OwnerSid -ne $ExpectedOwnerSid -or $Rules.Count -ne 3) {
        return $false
    }
    $actualSids = @($Rules | ForEach-Object { $_.IdentityReference.Value })
    if (@($AllowedSids | Where-Object { $actualSids -notcontains $_ }).Count -ne 0) {
        return $false
    }
    $violations = @($Rules | Where-Object {
        $_.AccessControlType -ne 'Allow' -or
        $AllowedSids -notcontains $_.IdentityReference.Value -or
        ($_.FileSystemRights -band [Security.AccessControl.FileSystemRights]::FullControl) -ne
            [Security.AccessControl.FileSystemRights]::FullControl
    })
    return $violations.Count -eq 0
}

if ($AclPredicateSelfTest) {
    $sid = 'S-1-5-21-1-2-3-1001'
    $allowed = @($sid, 'S-1-5-18', 'S-1-5-32-544')
    $rules = @($allowed | ForEach-Object {
        [pscustomobject]@{
            AccessControlType = 'Allow'
            IdentityReference = [pscustomobject]@{ Value = $_ }
            FileSystemRights = [Security.AccessControl.FileSystemRights]::FullControl
        }
    })
    if ($AclPredicateSelfTest -eq 'foreign') {
        $rules[-1].IdentityReference = [pscustomobject]@{ Value = 'S-1-1-0' }
    }
    if ($AclPredicateSelfTest -eq 'duplicate') {
        $rules[-1].IdentityReference = [pscustomobject]@{ Value = $sid }
    }
    $owner = if ($AclPredicateSelfTest -eq 'wrong-owner') { 'S-1-1-0' } else { $sid }
    $isProtected = $AclPredicateSelfTest -ne 'unprotected'
    [bool](Test-ExclusiveRuleSet $owner $sid $rules $allowed $isProtected)
    exit 0
}

$origin = 'https://github.com/ShaftHQ/SHAFT_ENGINE'
$trustModel = 'exclusive-maintenance-home-v1'
$taskName = 'SHAFT-Nightly-Knowledge-Refresh'
$sourceController = Join-Path $PSScriptRoot 'shaft_knowledge_refresh.py'
$sourceWrapper = Join-Path $PSScriptRoot 'graphify-refresh.cmd'
foreach ($name in @('GIT_DIR', 'GIT_COMMON_DIR', 'GIT_WORK_TREE', 'GIT_INDEX_FILE',
        'GIT_OBJECT_DIRECTORY', 'GIT_ALTERNATE_OBJECT_DIRECTORIES', 'GIT_CONFIG_COUNT',
        'GIT_CONFIG_SYSTEM', 'GIT_CONFIG_GLOBAL', 'GIT_CONFIG_PARAMETERS')) {
    Remove-Item -LiteralPath "Env:$name" -ErrorAction SilentlyContinue
}
Get-ChildItem Env: | Where-Object Name -Match '^GIT_CONFIG_(KEY|VALUE)_' | Remove-Item
$env:GIT_OPTIONAL_LOCKS = '0'
$env:GIT_CONFIG_NOSYSTEM = '1'
$env:GIT_CONFIG_GLOBAL = 'NUL'

$lexicalHome = [IO.Path]::GetFullPath($MaintenanceHome)
if ($lexicalHome -eq [IO.Path]::GetPathRoot($lexicalHome)) { throw 'Maintenance home must be a dedicated directory, not a drive root.' }
$lexicalHome = $lexicalHome.TrimEnd('\')
$lexicalRoot = Join-Path $lexicalHome 'SHAFT_ENGINE-main'
$lexicalSentinel = Join-Path $lexicalHome '.shaft-nightly-maintenance.json'
py -3 $sourceController --root $lexicalRoot --sentinel $lexicalSentinel --validate-home-only
if ($LASTEXITCODE -ne 0) { throw 'Maintenance-home preflight failed before the first write.' }

$currentSid = [Security.Principal.WindowsIdentity]::GetCurrent().User.Value
$allowedSids = @($currentSid, 'S-1-5-18', 'S-1-5-32-544')
function New-ExclusiveDirectorySecurity {
    $security = [Security.AccessControl.DirectorySecurity]::new()
    $security.SetAccessRuleProtection($true, $false)
    $security.SetOwner([Security.Principal.SecurityIdentifier]::new($currentSid))
    foreach ($sid in $allowedSids) {
        $security.AddAccessRule([Security.AccessControl.FileSystemAccessRule]::new(
            [Security.Principal.SecurityIdentifier]::new($sid),
            [Security.AccessControl.FileSystemRights]::FullControl,
            [Security.AccessControl.InheritanceFlags]'ContainerInherit, ObjectInherit',
            [Security.AccessControl.PropagationFlags]::None,
            [Security.AccessControl.AccessControlType]::Allow)) | Out-Null
    }
    return $security
}
function New-ExclusiveDirectory([string]$Path) {
    $security = New-ExclusiveDirectorySecurity
    [ShaftMaintenance.SecureDirectory]::Create($Path, $security.GetSecurityDescriptorBinaryForm())
}
function Assert-ExclusiveMaintenanceAcl([string]$Path) {
    $aclRoot = (Get-Item -LiteralPath $Path -Force).FullName
    foreach ($target in @((Get-Item -LiteralPath $aclRoot -Force)) + @(Get-ChildItem -LiteralPath $aclRoot -Force -Recurse)) {
        $acl = Get-Acl -LiteralPath $target.FullName
        $rules = @($acl.GetAccessRules($true, $true, [Security.Principal.SecurityIdentifier]))
        $owner = $acl.GetOwner([Security.Principal.SecurityIdentifier]).Value
        $protectionIsValid = $target.FullName -ne $aclRoot -or $acl.AreAccessRulesProtected
        if (-not (Test-ExclusiveRuleSet $owner $currentSid $rules $allowedSids $protectionIsValid)) {
            throw "Maintenance ACL verification failed: $($target.FullName)"
        }
    }
}

function Assert-ControllerBundle(
    [string]$BundleHome,
    [string]$ExpectedControllerDigest,
    [string]$ExpectedWrapperDigest
) {
    $controller = Join-Path $BundleHome 'shaft_knowledge_refresh.py'
    $wrapper = Join-Path $BundleHome 'graphify-refresh.cmd'
    $entries = @(Get-ChildItem -LiteralPath $BundleHome -Force)
    $entryNames = @($entries | ForEach-Object Name | Sort-Object)
    if (
        $entries.Count -ne 2 -or
        ($entryNames -join '|') -ne 'graphify-refresh.cmd|shaft_knowledge_refresh.py' -or
        -not (Test-Path -LiteralPath $controller -PathType Leaf) -or
        -not (Test-Path -LiteralPath $wrapper -PathType Leaf) -or
        (Get-FileHash -Algorithm SHA256 $controller).Hash -ne $ExpectedControllerDigest -or
        (Get-FileHash -Algorithm SHA256 $wrapper).Hash -ne $ExpectedWrapperDigest
    ) {
        throw 'Versioned controller bundle is incomplete or does not match its identity.'
    }
}

function Install-ControllerBundle(
    [string]$ControllerRoot,
    [string]$BundleHash,
    [string]$ControllerSource,
    [string]$WrapperSource,
    [string]$ExpectedControllerDigest,
    [string]$ExpectedWrapperDigest
) {
    $final = Join-Path $ControllerRoot $BundleHash
    foreach ($orphan in @(Get-ChildItem -LiteralPath $ControllerRoot -Directory -Force -Filter "$BundleHash.staging-*")) {
        Assert-ExclusiveMaintenanceAcl $orphan.FullName
        Remove-Item -LiteralPath $orphan.FullName -Recurse -Force
    }
    if (Test-Path -LiteralPath $final) {
        Assert-ControllerBundle $final $ExpectedControllerDigest $ExpectedWrapperDigest
        return $final
    }
    $staging = "$final.staging-$([guid]::NewGuid().ToString('N'))"
    try {
        New-ExclusiveDirectory $staging
        Copy-Item -LiteralPath $ControllerSource -Destination (Join-Path $staging 'shaft_knowledge_refresh.py')
        Copy-Item -LiteralPath $WrapperSource -Destination (Join-Path $staging 'graphify-refresh.cmd')
        Assert-ExclusiveMaintenanceAcl $staging
        Assert-ControllerBundle $staging $ExpectedControllerDigest $ExpectedWrapperDigest
        try {
            [IO.Directory]::Move($staging, $final)
        } catch [IO.IOException] {
            if (-not (Test-Path -LiteralPath $final -PathType Container)) { throw }
            Assert-ControllerBundle $final $ExpectedControllerDigest $ExpectedWrapperDigest
        }
    } finally {
        if (Test-Path -LiteralPath $staging) {
            Remove-Item -LiteralPath $staging -Recurse -Force
        }
    }
    Assert-ControllerBundle $final $ExpectedControllerDigest $ExpectedWrapperDigest
    return $final
}

function Open-InstallerLock([string]$MaintenanceHome) {
    $lockPath = Join-Path $MaintenanceHome '.shaft-install.lock'
    try {
        return [IO.File]::Open(
            $lockPath,
            [IO.FileMode]::OpenOrCreate,
            [IO.FileAccess]::ReadWrite,
            [IO.FileShare]::None)
    } catch [IO.IOException] {
        throw 'SHAFT nightly installer is already running.'
    }
}

function Invoke-WithInstallerLock([string]$MaintenanceHome, [scriptblock]$Action) {
    $lock = Open-InstallerLock $MaintenanceHome
    try {
        & $Action
    } finally {
        $lock.Dispose()
    }
}

if ($SecureDirectorySelfTest) {
    New-ExclusiveDirectory $SecureDirectorySelfTest
    Assert-ExclusiveMaintenanceAcl $SecureDirectorySelfTest
    $true
    exit 0
}

if ($BundlePromotionSelfTest) {
    $controllerRoot = Join-Path $BundlePromotionSelfTest 'Controller'
    New-ExclusiveDirectory $controllerRoot
    $controllerDigest = (Get-FileHash -Algorithm SHA256 $sourceController).Hash
    $wrapperDigest = (Get-FileHash -Algorithm SHA256 $sourceWrapper).Hash
    $bundleHash = Get-ControllerBundleHash $controllerDigest $wrapperDigest
    $orphan = Join-Path $controllerRoot "$bundleHash.staging-orphan"
    New-ExclusiveDirectory $orphan
    $bundle = Install-ControllerBundle $controllerRoot $bundleHash $sourceController $sourceWrapper $controllerDigest $wrapperDigest
    [bool](Test-Path -LiteralPath (Join-Path $bundle 'graphify-refresh.cmd') -PathType Leaf)
    exit 0
}

if ($InstallerLockSelfTest) {
    if (-not (Test-Path -LiteralPath $InstallerLockSelfTest)) {
        New-ExclusiveDirectory $InstallerLockSelfTest
    } else {
        Assert-ExclusiveMaintenanceAcl $InstallerLockSelfTest
    }
    $selfTestLock = Open-InstallerLock $InstallerLockSelfTest
    try {
        if ($InstallerLockHoldMilliseconds -gt 0) {
            Start-Sleep -Milliseconds $InstallerLockHoldMilliseconds
        }
        $true
    } finally {
        $selfTestLock.Dispose()
    }
    exit 0
}

if ($InstallerLockFailureSelfTest) {
    New-ExclusiveDirectory $InstallerLockFailureSelfTest
    try {
        Invoke-WithInstallerLock $InstallerLockFailureSelfTest { throw 'injected failure' }
    } catch {
        if ($_.Exception.Message -ne 'injected failure') { throw }
    }
    $retry = Open-InstallerLock $InstallerLockFailureSelfTest
    try { $true } finally { $retry.Dispose() }
    exit 0
}

if (-not (Test-Path -LiteralPath $lexicalHome)) {
    New-ExclusiveDirectory $lexicalHome
} else {
    Assert-ExclusiveMaintenanceAcl $lexicalHome
}
$homePath = (Get-Item -LiteralPath $lexicalHome -Force).FullName.TrimEnd('\')
Invoke-WithInstallerLock $homePath {
Assert-ExclusiveMaintenanceAcl $homePath
$root = Join-Path $homePath 'SHAFT_ENGINE-main'
$sentinel = Join-Path $homePath '.shaft-nightly-maintenance.json'
$pending = Join-Path $homePath '.shaft-install-pending.json'
$logs = Join-Path $homePath 'Logs'

py -3 $sourceController --root $root --sentinel $sentinel --validate-home-only
if ($LASTEXITCODE -ne 0) { throw 'Maintenance home changed after ACL establishment.' }
New-Item -ItemType Directory -Force $logs | Out-Null

$controllerDigest = (Get-FileHash -Algorithm SHA256 $sourceController).Hash
$wrapperDigest = (Get-FileHash -Algorithm SHA256 $sourceWrapper).Hash
$controllerHash = Get-ControllerBundleHash $controllerDigest $wrapperDigest
$controllerRoot = Join-Path $homePath 'Controller'
if (-not (Test-Path -LiteralPath $controllerRoot)) {
    New-ExclusiveDirectory $controllerRoot
}
Assert-ExclusiveMaintenanceAcl $homePath
$controllerHome = Join-Path $controllerRoot $controllerHash
$controllerHome = Install-ControllerBundle $controllerRoot $controllerHash $sourceController $sourceWrapper $controllerDigest $wrapperDigest
Assert-ExclusiveMaintenanceAcl $homePath
$installedController = Join-Path $controllerHome 'shaft_knowledge_refresh.py'
$installedWrapper = Join-Path $controllerHome 'graphify-refresh.cmd'

if (-not (Test-Path -LiteralPath $sentinel -PathType Leaf)) {
    if (Test-Path -LiteralPath $pending -PathType Leaf) {
        py -3 $installedController --root $root --sentinel $sentinel --validate-pending $pending
        if ($LASTEXITCODE -ne 0) { throw 'Pending install receipt cannot authorize recovery.' }
        $pendingReceipt = Get-Content -LiteralPath $pending -Raw | ConvertFrom-Json
        $token = $pendingReceipt.owner_token
        if (Test-Path -LiteralPath $root) {
            py -3 $installedController --root $root --sentinel $sentinel --validate-home-only
            if ($LASTEXITCODE -ne 0) { throw 'Interrupted clone path is unsafe to recover.' }
            Remove-Item -LiteralPath $root -Recurse -Force
        }
        Write-Output 'Recovering installer-owned clone from the independent pending receipt.'
    } else {
        if (Test-Path -LiteralPath $root) { throw 'Existing clone has no independent installer receipt.' }
        $token = [guid]::NewGuid().ToString('N')
        $pendingReceipt = [ordered]@{
            schema_version = 1; repository_root = $root; origin = $origin
            owner_token = $token; trust_model = $trustModel
        }
        Write-Utf8NoBomJson $pendingReceipt $pending
        py -3 $installedController --root $root --sentinel $sentinel --validate-pending $pending
        if ($LASTEXITCODE -ne 0) { throw 'New pending receipt failed its own validation.' }
    }
    git clone -c shaft.maintenanceOwner=$token --origin origin --branch main --single-branch $origin $root
    if ($LASTEXITCODE -ne 0) { throw 'git clone failed; the pending receipt permits a safe retry.' }
    py -3 $installedController --root $root --sentinel $sentinel --validate-paths-only
    if ($LASTEXITCODE -ne 0) { throw 'Fresh clone failed the owned-path preflight.' }
    $head = (git -C $root rev-parse HEAD).Trim()
    if ($LASTEXITCODE -ne 0) { throw 'Cannot read the fresh clone revision.' }
    $remoteLine = (git ls-remote --exit-code $origin refs/heads/main).Trim()
    if ($LASTEXITCODE -ne 0) { throw 'Cannot verify approved remote main.' }
    if ($head -ne (($remoteLine -split '\s+')[0])) { throw 'Approved main changed during clone.' }
    $receipt = [ordered]@{
        schema_version = 1; repository_root = $root; origin = $origin
        owner_token = $token; trust_model = $trustModel
    }
    Write-Utf8NoBomJson $receipt "$sentinel.tmp"
    Move-Item -LiteralPath "$sentinel.tmp" -Destination $sentinel
    Remove-Item -LiteralPath $pending -Force
}

py -3 $installedController --root $root --sentinel $sentinel --validate-only
if ($LASTEXITCODE -ne 0) { throw 'Installer-owned clone failed the external controller preflight.' }
if (Test-Path -LiteralPath $pending -PathType Leaf) {
    py -3 $installedController --root $root --sentinel $sentinel --validate-pending $pending
    if ($LASTEXITCODE -ne 0) { throw 'Stale pending receipt is invalid.' }
    $pendingReceipt = Get-Content -LiteralPath $pending -Raw | ConvertFrom-Json
    $finalReceipt = Get-Content -LiteralPath $sentinel -Raw | ConvertFrom-Json
    if ($pendingReceipt.owner_token -ne $finalReceipt.owner_token) { throw 'Stale pending receipt conflicts with the finalized installation.' }
    Remove-Item -LiteralPath $pending -Force
}
$action = New-ScheduledTaskAction -Execute $env:ComSpec -Argument ('/d /c ""{0}""' -f $installedWrapper) -WorkingDirectory $controllerHome
$trigger = New-ScheduledTaskTrigger -Daily -At '05:00'
$settings = New-ScheduledTaskSettingsSet -StartWhenAvailable -RunOnlyIfNetworkAvailable -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -MultipleInstances IgnoreNew -ExecutionTimeLimit (New-TimeSpan -Hours 3) -RestartCount 3 -RestartInterval (New-TimeSpan -Minutes 15)
$principal = New-ScheduledTaskPrincipal -UserId ([Security.Principal.WindowsIdentity]::GetCurrent().Name) -LogonType Interactive -RunLevel Limited
Register-ScheduledTask -TaskName $taskName -Action $action -Trigger $trigger -Settings $settings -Principal $principal -Description 'Refresh SHAFT Graphify and MemPalace from verified origin/main when Mohab is logged in.' -Force | Out-Null
$legacyTasks = @(Get-ScheduledTask -ErrorAction Stop | Where-Object {
    $_.TaskName -eq 'graphify-refresh' -and $_.TaskPath -eq '\'
})
if ($legacyTasks.Count -gt 1) { throw 'Multiple root legacy graphify-refresh tasks were returned.' }
if ($legacyTasks.Count -eq 1) {
    $legacyTasks[0] | Unregister-ScheduledTask -Confirm:$false -ErrorAction Stop
}
Write-Output "Registered $taskName using versioned protected controller $installedController."
}
