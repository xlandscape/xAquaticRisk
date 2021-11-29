@echo off
set username=SMULTSCH.KNOELL.COM
set token=znBAekzLdJ_3FFSD4b96
set project=FocusMacro-Component
git clone --recurse-submodules https://%username%:%token%@gitlab.bayer.com/aqrisk-landscape/%project%.git FocusMacro
pause


