I received an email that I need to fix the Suggests issue. I have fixed that
issue.

## Test environments
* local Windows 11 install, R 4.4.1
* Ubuntu 22.04 (on Github Actions), R 4.3.2
* win-builder (devel and release)
* mac-builder
* R-hub

## R CMD check results

On Windows 11 (1/20/24): 0 errors/warnings/notes

win-builder (check_win_release, 1/19/24): "Status: OK"

win-builder(check_win_devel, 1/20/24): "Status: OK"

R-hub:
Windows Server 2022 (1/19/24): 2 NOTEs, neither are a real issue.
Fedora Linux (1/20/24): 2 NOTEs, one is a slow example, the other isn't a real issue.
Ubuntu Linux (1/20/24): 2 NOTEs, one is a slow example, the other isn't a real issue.

On GitHub Actions (1/20/24): "Status: OK"

## Downstream dependencies

There is one reverse import: optimizeR

I used revdepcheck, and it had status OK with no errors/warnings/notes.
