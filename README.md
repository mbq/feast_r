# praznik

Praznik provides an R interface to the [FEAST](https://github.com/Craigacp/FEAST) features selection library, providing several information-based filter methods.

This branch contains static copies of the required subset of FEAST and its helper library, [MIToolbox](https://github.com/Craigacp/MIToolbox).

You can compile and install with `R CMD ...` or with `devtools::install()`; `devtools::install_github()` should also work.

Exported functions: `CondMI`, `JMI`, `mRMR_D`, `DISR`, `MIM`, `ICAP` and `CMIM`.
Consult the included R manuals for usage.

License: 3-clause BSD
