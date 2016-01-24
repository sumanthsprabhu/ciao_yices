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
# TODO: Allow compilation from source
case "$CIAO_OS" in
    LINUX)
	yices_file="$yices_name""-x86_64-unknown-linux-gnu-static-gmp.tar.gz" ;;
    DARWIN)
	yices_file="$yices_name""-x86_64-apple-darwin15.2.0-static-gmp.tar.gz" ;;
    *)
	echo "ERROR: Unsupported CIAO_OS=$CIAO_OS" 1>&2
	exit 1
esac
yices_url="http://yices.csl.sri.com/cgi-bin/yices2-newnewdownload.cgi?file=$yices_file&accept=I+Agree"

# ---------------------------------------------------------------------------

# Default compiler
case "$CIAO_OS" in
    LINUX)  CXX="g++" ;;
    DARWIN) CXX="clang++" ;;
    *)
	echo "ERROR: Unsupported CIAO_OS=$CIAO_OS" 1>&2
	exit 1
esac
case "$CIAO_ARCH" in
    i686)   CXXFLAGS="-m32" ;;
    x86_64) CXXFLAGS="" ;;
    *)
	echo "ERROR: Unsupported CIAO_ARCH=$CIAO_ARCH" 1>&2
	exit 1
esac

# --------------------------------------------------------------------------

cachedir="$bdlroot/third-party/cache"
storedir="$bdlroot/third-party/store"

# --------------------------------------------------------------------------

function fetch_yices() {
    if [ -x "$storedir/$yices_name" ]; then
	# echo "yices already downloaded" 1>&2
	return 0
    fi

    # Ensure that cachedir is created
    mkdir -p "$cachedir"

    # Download yices
    rm -f "$cachedir/$yices_file"
    curl "$yices_url" -o "$cachedir/$yices_file"

    # Cleanup storedir for yices and uncompress
    rm -rf "$storedir/$yices_name"
    mkdir -p "$storedir/$yices_name"
    tar -xz --strip-components 1 -f "$cachedir/$yices_file" -C "$storedir/$yices_name"

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
	    RPATH="-Wl,-rpath,$storedir/$yices_name/lib"
	    ;;
    esac
    cat > $bdlroot/src/ciao_yices_config_auto.pl <<EOF
:- extra_linker_opts(' -L.').
:- extra_linker_opts([
	% For Yices
	'$RPATH -L$storedir/$yices_name/lib'
	]).

:- use_foreign_library(['yices']).
EOF
}

# ===========================================================================

case $1 in
    fetch) fetch_yices ;;
    gen_conf) gen_config_auto ;;
    *)
	echo "ERROR: Unknown action" 1>&2
	exit 1
esac
