#!/bin/sh

# Exit immediately if a simple command exits with a non-zero status
set -e

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

old_dir=`pwd`; cd "$_base/.."; bdlroot=`pwd`; cd "$old_dir"; old_dir=

# ---------------------------------------------------------------------------
# Configuration

yices_name="yices-2.4.2"
# TODO: Select based on OS/ARCH
yices_file="$yices_name""-x86_64-apple-darwin15.2.0-static-gmp.tar.gz"
yices_url="http://yices.csl.sri.com/cgi-bin/yices2-newnewdownload.cgi?file=$yices_file&accept=I+Agree"

# ---------------------------------------------------------------------------

if ciaoroot=`which ciao`; then
    ciaoroot=$(dirname $(dirname $(dirname "$ciaoroot")))
else
    echo "ERROR: No ciao installation in path" 1>&2
    exit 1
fi
# Detect the OS/ARCH at configuration
eng_cfg=`. "$ciaoroot"/build/ciao.config_saved_sh; echo $core__OS$core__ARCH`
# cat "$ciaoroot"/build/eng/ciaoengine/cfg/"$eng_cfg"/config_sh

# Default compiler
case "$eng_cfg" in
    LINUX*)  CXX="g++" ;;
    DARWIN*) CXX="clang++" ;;
    *)
	echo "ERROR: Unsupported $eng_cfg" 1>&2
	exit 1
esac
case "$eng_cfg" in
    *i686)   CXXFLAGS="-m32" ;;
    *x86_64) CXXFLAGS="" ;;
esac
CIAO_INCLUDE="$ciaoroot/build/eng/ciaoengine/include"

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
    local yiceslibVer="$storedir/$yices_name/lib/libyices.2.dylib"
    local yiceslib="$storedir/$yices_name/lib/libyices.dylib"
    # Fix install dir (it was /usr/local)
    install_name_tool -id "$yiceslibVer" "$yiceslibVer"
    # Link name without version
    ln -sf "$yiceslibVer" "$yiceslib"
}

# ---------------------------------------------------------------------------

function gen_config_auto() {
    cat > $bdlroot/src/ciao_yices_config_auto.pl <<EOF
:- extra_linker_opts(' -L.').
:- extra_linker_opts([
	% For Yices
	' -L$storedir/$yices_name/lib'
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
