@echo off
set username=SMULTSCH.KNOELL.COM
set token=znBAekzLdJ_3FFSD4b96
set project=CmfContinuous-Component
git clone --recurse-submodules --single-branch --branch integration_drainage https://%username%:%token%@gitlab.bayer.com/aqrisk-landscape/%project%.git CmfContinuous
pause


