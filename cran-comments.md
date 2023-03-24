## Test environments

- local macOS R installation, R 4.2.3
- continuous integration via GH actions:
    - macOS latest release
    - windows latest release
    - linux unbuntu release
    - ubuntu 20.04 latest release and devel
- win-builder (release and devel)
- macOS-builder
- R-hub
    - Windows Server 2022, R-devel, 64 bit
    - Ubuntu Linux 20.04.1 LTS, R-release, GCC
    - Fedora Linux, R-devel, clang, gfortran

## R CMD check results

0 errors | 0 warnings | 2 notes

    * checking HTML version of manual ... [13s] NOTE
    
    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'
