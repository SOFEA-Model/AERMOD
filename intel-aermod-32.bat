@REM                                                                    + + +
@echo off

setlocal

set COMPILE_FLAGS=/O2 /DENABLE_NETCDF /DENABLE_IPC /check:format /Qprec-div- /QaxSSE2 /trace /Qdiag-disable:8291 /I"D:\Lib\netcdf-fortran-4.4.4-x32\include"
set LINK_FLAGS=/O2 /Qipo- /check:format /Qprec-div- /QaxSSE2
set LINKER_FLAGS=^
 /LIBPATH:"D:\Lib\hdf5-1.8.21-vc141-mt-x32\lib"^
 /LIBPATH:"D:\Lib\zlib-1.2.11-vc141-mt-x32\lib"^
 /LIBPATH:"D:\Lib\szip-2.1.1-vc141-mt-x32\lib"^
 /LIBPATH:"D:\Lib\curl-7.62.0-vc141-mt-x32\lib"^
 /LIBPATH:"D:\Lib\netcdf-c-4.6.2-vc141-mt-x32\lib"^
 /LIBPATH:"D:\Lib\netcdf-fortran-4.4.4-x32\lib"^
 crypt32.lib wldap32.lib normaliz.lib wsock32.lib ws2_32.lib^
 zlib.lib libszip.lib libhdf5.lib libhdf5_hl.lib libcurl_a.lib netcdf.lib netcdff.lib^
 libucrt.lib libmmt.lib

ifort /compile_only %COMPILE_FLAGS% modules.f
ifort /fpp /compile_only %COMPILE_FLAGS% ncpost.f
ifort /fpp /compile_only %COMPILE_FLAGS% ipc.f
ifort /compile_only %COMPILE_FLAGS% aermod.f
ifort /compile_only %COMPILE_FLAGS% setup.f
ifort /compile_only %COMPILE_FLAGS% coset.f
ifort /compile_only %COMPILE_FLAGS% soset.f
ifort /compile_only %COMPILE_FLAGS% reset.f
ifort /compile_only %COMPILE_FLAGS% meset.f
ifort /compile_only %COMPILE_FLAGS% ouset.f
ifort /compile_only %COMPILE_FLAGS% inpsum.f
ifort /compile_only %COMPILE_FLAGS% metext.f
ifort /compile_only %COMPILE_FLAGS% iblval.f
ifort /compile_only %COMPILE_FLAGS% siggrid.f
ifort /compile_only %COMPILE_FLAGS% tempgrid.f
ifort /compile_only %COMPILE_FLAGS% windgrid.f
ifort /compile_only %COMPILE_FLAGS% calc1.f
ifort /compile_only %COMPILE_FLAGS% calc2.f
ifort /compile_only %COMPILE_FLAGS% prise.f
ifort /compile_only %COMPILE_FLAGS% prime.f
ifort /compile_only %COMPILE_FLAGS% sigmas.f
ifort /compile_only %COMPILE_FLAGS% pitarea.f
ifort /compile_only %COMPILE_FLAGS% uninam.f
ifort /compile_only %COMPILE_FLAGS% output.f
ifort /compile_only %COMPILE_FLAGS% evset.f
ifort /compile_only %COMPILE_FLAGS% evcalc.f
ifort /compile_only %COMPILE_FLAGS% evoutput.f

ifort /exe:bin32/aermod_netcdf.exe %LINK_FLAGS%^
 MODULES.obj NCPOST.obj IPC.obj AERMOD.obj SETUP.obj COSET.obj SOSET.obj^
 RESET.obj MESET.obj OUSET.obj INPSUM.obj METEXT.obj^
 IBLVAL.obj SIGGRID.obj TEMPGRID.obj WINDGRID.obj CALC1.obj^
 CALC2.obj PRISE.obj PRIME.obj SIGMAS.obj PITAREA.obj^
 UNINAM.obj OUTPUT.obj EVSET.obj EVCALC.obj EVOUTPUT.obj^
 /link %LINKER_FLAGS%

del *.obj
del *.mod
