#!/bin/bash

# Exit immediately if a simple command exits with a non-zero status
set -e

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

old_dir=`pwd`; cd "$_base/.."; bdlroot=`pwd`; cd "$old_dir"; old_dir=

# ---------------------------------------------------------------------------
# Configuration

yices_name="yices-2.4.2"

function select_bin_dist() {
    case "$CIAO_OS" in
	LINUX)
	    yices_file="$yices_name""-x86_64-unknown-linux-gnu-static-gmp.tar.gz" ;;
	DARWIN)
	    yices_file="$yices_name""-x86_64-apple-darwin15.2.0-static-gmp.tar.gz" ;;
	*)
	    echo "ERROR: Unsupported CIAO_OS=$CIAO_OS" 1>&2
	    exit 1
    esac
}

function select_src_dist() {
    yices_file="yices-2.4.2-src.tar.gz"
}

# --------------------------------------------------------------------------

if [ "$THIRDPARTY" = "" ]; then
    cat <<EOF
ERROR: THIRDPARTY directory missing (use 'ciao build')
EOF
    exit 1
fi

cachedir="$THIRDPARTY/cache"
storedir="$THIRDPARTY/store"
srcdir="$THIRDPARTY/src"

# --------------------------------------------------------------------------

function fetch_yices() {
    # Ensure that cachedir is created
    mkdir -p "$cachedir"

    # Download yices
    rm -f "$cachedir/$yices_file"
    yices_url="http://yices.csl.sri.com/cgi-bin/yices2-newnewdownload.cgi?file=$yices_file&accept=I+Agree"
    curl "$yices_url" -o "$cachedir/$yices_file"
}

function uncompress_yices_bin() {
    # Cleanup storedir for yices and uncompress
    rm -rf "$storedir/$yices_name"
    mkdir -p "$storedir/$yices_name"
    tar -xz --strip-components 1 -f "$cachedir/$yices_file" -C "$storedir/$yices_name"
}

function uncompress_yices_src() {
    # Cleanup srcdir for yices and uncompress
    rm -rf "$srcdir/$yices_name"
    mkdir -p "$srcdir/$yices_name"
    tar -xz --strip-components 1 -f "$cachedir/$yices_file" -C "$srcdir/$yices_name"
}

function build_yices() {
    pushd "$srcdir/$yices_name" > /dev/null 2>&1

    LDFLAGS="-L$THIRDPARTY/lib" CPPFLAGS="-I$THIRDPARTY/include" LD_LIBRARY_PATH="$THIRDPARTY/lib" ./configure
    make
    
    # Cleanup storedir for yices and copy
    rm -rf "$storedir/$yices_name"
    mkdir -p "$storedir/$yices_name"
    cp -R "$srcdir/$yices_name/build/"*"/dist/"* "$storedir/$yices_name/"
    
    popd > /dev/null 2>&1
}

function fix_dylibs() {
    # TODO: We do not use install-yices, just link ourselves the lib
    local yiceslibVerN yiceslibN
    case "$CIAO_OS" in
	LINUX)
	    yiceslibVerN="libyices.so.2.4.2"
	    yiceslibN="libyices.so"
	    ;;
	DARWIN)
	    yiceslibVerN="libyices.2.dylib"
	    yiceslibN="libyices.dylib"
	    ;;
    esac
    local yiceslibVer="$storedir/$yices_name/lib/$yiceslibVerN"
    local yiceslib="$storedir/$yices_name/lib/$yiceslibN"
    # Fix install dir (it was /usr/local)
    case "$CIAO_OS" in
	LINUX)
	    pushd "$storedir/$yices_name" > /dev/null 2>&1
	    /sbin/ldconfig -n "lib"
            # Link name without version
	    ln -sf "$yiceslibVerN" "lib/$yiceslibN"
	    popd > /dev/null 2>&1
	    ;;
	DARWIN)
	    install_name_tool -id "$yiceslibVer" "$yiceslibVer"
            # Link name without version
	    ln -sf "$yiceslibVer" "$yiceslib"
	    ;;
    esac
}

# ---------------------------------------------------------------------------

function gen_config_auto() {
    local RPATH=
    case "$CIAO_OS" in
	LINUX)
	    RPATH="-Wl,-rpath,$storedir/$yices_name/lib,-rpath,\\'\$ORIGIN\\'"
	    ;;
    esac
    cat > $bdlroot/src/ciao_yices_config_auto.pl <<EOF
:- extra_compiler_opts([
	% For Yices
	'-I$storedir/$yices_name/include'
	]).
:- extra_linker_opts(' -L.').
:- extra_linker_opts([
	% For Yices
	'$RPATH -L$storedir/$yices_name/lib'
	]).

:- use_foreign_library(['yices']).
EOF
}

# ===========================================================================

function install_dist() { # Mode=bin|src
    if [ -x "$storedir/$yices_name" ]; then
	# echo "yices already downloaded" 1>&2
	return 0
    fi

    if [ "$1" = bin ]; then
	select_bin_dist
    else # src
	select_src_dist
    fi
    fetch_yices
    if [ "$1" = bin ]; then
	uncompress_yices_bin
    else # src
	uncompress_yices_src
	build_yices
    fi
    fix_dylibs
}

# ===========================================================================

case $1 in
    install_bin_dist) install_dist bin ;;
    install_src_dist) install_dist src ;;
    gen_conf) gen_config_auto ;;
    *)
	echo "ERROR: Unknown action" 1>&2
	exit 1
esac
