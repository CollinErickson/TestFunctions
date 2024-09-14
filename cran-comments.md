I received an email that I need to fix the Suggests issue. I have fixed that
issue.

## Test environments
* local Windows 11 install, R 4.4.1
* Ubuntu 22.04 (on Github Actions), R 4.4.1
* win-builder (devel and release)
* mac-builder
* R-hub (5 systems)

## R CMD check results

On Windows 11, R 4.4.1 (9/13/24): 0 errors/warnings/notes
Both regular and with _R_CHECK_DEPENDS_ONLY_=TRUE

mac-builder, R 4.4.0 (9/13/24): 1 WARNING
There's a warning due to my other package that was just updated on CRAN.
Warning: package ‘ContourFunctions’ was built under R version 4.4.1

win-builder (check_win_release, 9/13/24): OK

win-builder(check_win_devel, 9/13/24): OK

R-hub:
ubuntu-next, Ubuntu 22.04.4 (9/13/24): OK
valgrind, Ubuntu 22.04.4 (9/13/24): OK
linux (R-devel), Ubuntu 22.04.4 (9/13/24): OK
macos (R-devel), macOS 13.6.9 (9/13/24): OK
windows (R-devel), Microsoft Windows Server 2022 (9/13/24): OK

On GitHub Actions (9/13/24): OK

## Downstream dependencies

There is one reverse import: optimizeR

This fix will get make sure that optimizeR still works. I couldn't get
revdepcheck to work, but I cloned the GitHub repo and ran check on my laptop
and it passed with no issues.
