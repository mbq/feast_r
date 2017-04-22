# praznik

Praznik provides an R interface to the [FEAST](https://github.com/Craigacp/FEAST) features selection library, providing several information-based filter methods.

To build, clone the repository, and run
```
git submodule init
git submodule update
```
to download the latest version of FEAST and its helper library, [MIToolbox](https://github.com/Craigacp/MIToolbox).

After this, you can compile and install with `R CMD ...` or with `devtools::install()`.
`install_github` won't work because of the use of submodules.

Exported functions: `CondMI`, `JMI`, `mRMR_D`, `DISR`, `MIM`, `ICAP` and `CMIM`.
Consult the included R manuals for usage.

License: 3-clause BSD
