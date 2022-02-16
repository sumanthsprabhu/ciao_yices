# Yices interface for Ciao

Interface to Yices SMT solver (>2.3.1) for Ciao.

Requirements:
 - A modern 64-bit compilation of Ciao (>=1.20.0)
 - GMP
 - GNU `gperf` (macOS: included in Xcode; Linux: `apt-get install gperf`)

# Build and installation

You can automatically fetch, build, and install this bundle using:

```
ciao get ciao_yices
```

Dependencies will be downloaded and built under the first directory
specified in the `CIAOPATH` environment variable or `~/.ciao` by default.

**For developing** it is recommended to define your own *workspace
directory* and clone this repository. E.g., `export CIAOPATH=~/ciao`
and update your `PATH` with `eval "$(ciao-env)"`. The dependencies can
be cloned manually or fetched automatically by calling `ciao fetch` at
the source directory.
