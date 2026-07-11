<#
.SYNOPSIS
Deploy the orgzly-native bundle (and optionally the companion APK) to the
connected Android device.

.DESCRIPTION
Default: rebuild orgzly.el from emacs/apps/orgzly/*.el via WSL Emacs, then
adb-push it to /sdcard/Download/orgzly.el. Termux is not debuggable, so adb
cannot write into /data/data/com.termux directly — pair this with the
jetpacs starter init (jetpacs/docs/starter-init.el), which adopts a newer
staged bundle at Emacs startup:

    ;; Adopt a freshly adb-pushed bundle before loading it.
    (let ((staged "/sdcard/Download/orgzly.el")
          (installed "~/.emacs.d/elisp/orgzly.el"))
      (when (and (file-readable-p staged)
                 (file-newer-than-file-p staged installed))
        (copy-file staged installed t)
        (message "orgzly: adopted new bundle from Downloads")))
    (require 'orgzly)

Works whether the repo lives on a Windows drive (C:\...) or inside WSL
(\\wsl.localhost\<distro>\...) — the bundle build always runs in WSL Emacs.

.PARAMETER Ssh
Push straight into Termux's home (~/.emacs.d/elisp/orgzly.el) over Termux
sshd — a true direct drop, no staging or restart-adopt needed.
One-time setup inside Termux:
    pkg install openssh && passwd && sshd
Optional, for passwordless pushes: append your Windows public key
(~/.ssh/id_ed25519.pub) to ~/.ssh/authorized_keys in Termux.
sshd must be running on the device when you deploy (`sshd` in Termux).

.PARAMETER Core
Also deploy the vendored jetpacs/jetpacs-core.el — the bundle `require`s
it, so use this for a first deploy or after a jetpacs update.

.PARAMETER Apk
Also build and install the companion app (jetpacs\gradlew installDebug).
#>
param(
    [switch]$Ssh,
    [switch]$Core,
    [switch]$Apk
)

$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
$bundle = Join-Path $repo 'orgzly.el'
$coreFile = Join-Path $repo 'jetpacs\jetpacs-core.el'

# Locate the repo as WSL sees it, and the distro to build in:
# \\wsl.localhost\<distro>\path (or \\wsl$\...) -> that distro, in-distro path;
# C:\path\to\repo -> the default distro, /mnt/c/path/to/repo.
if ($repo -match '^\\\\wsl(?:\.localhost|\$)\\([^\\]+)\\(.*)$') {
    $distro = $Matches[1]
    $wslRepo = '/' + ($Matches[2] -replace '\\', '/')
} else {
    $distro = 'Debian'
    $wslRepo = '/mnt/' + $repo.Substring(0, 1).ToLower() + ($repo.Substring(2) -replace '\\', '/')
}

Write-Host '-- Rebuilding orgzly.el from emacs/apps/orgzly/*.el ...'
wsl.exe -d $distro -- emacs --batch -l "$wslRepo/emacs/build-bundle.el"
if ($LASTEXITCODE -ne 0) { throw 'Bundle build failed.' }

Write-Host '-- Checking device ...'
adb get-state | Out-Null
if ($LASTEXITCODE -ne 0) { throw 'No device visible to adb.' }

if ($Ssh) {
    Write-Host '-- Pushing directly into Termux home via sshd (port 8022) ...'
    adb forward tcp:8022 tcp:8022 | Out-Null
    ssh -p 8022 termux@127.0.0.1 'mkdir -p .emacs.d/elisp'
    if ($LASTEXITCODE -ne 0) { throw 'ssh failed - is sshd running in Termux?' }
    scp -P 8022 $bundle termux@127.0.0.1:.emacs.d/elisp/orgzly.el
    if ($LASTEXITCODE -ne 0) { throw 'scp failed.' }
    if ($Core) {
        scp -P 8022 $coreFile termux@127.0.0.1:.emacs.d/elisp/jetpacs-core.el
        if ($LASTEXITCODE -ne 0) { throw 'scp (jetpacs-core) failed.' }
    }
    Write-Host '   Installed to ~/.emacs.d/elisp/ - reload or restart Emacs.'
} else {
    Write-Host '-- Staging to /sdcard/Download (adopted by the starter init on Emacs restart) ...'
    adb push $bundle /sdcard/Download/orgzly.el
    if ($LASTEXITCODE -ne 0) { throw 'adb push failed.' }
    if ($Core) {
        adb push $coreFile /sdcard/Download/jetpacs-core.el
        if ($LASTEXITCODE -ne 0) { throw 'adb push (jetpacs-core) failed.' }
    }
    Write-Host '   Staged. Restart Emacs on the device (or eval the adopt snippet) to pick it up.'
}

if ($Apk) {
    Write-Host '-- Building + installing the jetpacs companion APK ...'
    & (Join-Path $repo 'jetpacs\gradlew.bat') -p (Join-Path $repo 'jetpacs') installDebug
    if ($LASTEXITCODE -ne 0) { throw 'APK install failed.' }
}

Write-Host 'Deploy complete.'
