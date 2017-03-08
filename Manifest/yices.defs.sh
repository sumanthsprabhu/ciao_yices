# 3rd-party package definition

# Package name
pkg_name="yices-2.4.2"

# Origin for binary distribution
function pkg_bin_origin() {
    case "$CIAO_OS" in
	LINUX)
	    pkg_tarfile="$pkg_name""-x86_64-unknown-linux-gnu-static-gmp.tar.gz" ;;
	DARWIN)
	    pkg_tarfile="$pkg_name""-x86_64-apple-darwin15.2.0-static-gmp.tar.gz" ;;
	*)
	    echo "ERROR: Unsupported CIAO_OS=$CIAO_OS" 1>&2
	    exit 1
    esac
    pkg_url="http://yices.csl.sri.com/cgi-bin/yices2-newnewdownload.cgi?file=$pkg_tarfile&accept=I+Agree"
}

# Origin for source distribution
function pkg_src_origin() {
    pkg_tarfile="yices-2.4.2-src.tar.gz"
    pkg_url="http://yices.csl.sri.com/cgi-bin/yices2-newnewdownload.cgi?file=$pkg_tarfile&accept=I+Agree"
}

# Fixes for binary distribution
function pkg_fix_bin() {
    true
}

# Build from source
function pkg_build() {
    LDFLAGS="-L$THIRDPARTY/lib" CPPFLAGS="-I$THIRDPARTY/include" LD_LIBRARY_PATH="$THIRDPARTY/lib" ./configure
    make
}

# Install from source
function pkg_install() {
    # NOTE: We do not use install-yices (our fix_dylib fixes dylib paths)
    cp -R "$srcdir/$pkg_name/build/"*"/dist/"* "$storedir/$pkg_name/"
}

# Dynamic library name and files
pkg_lib=yices
case "$CIAO_OS" in
    LINUX)
	pkg_libfile="libyices.so.2.4.2"
	pkg_libfileAct="libyices.so"
	;;
    DARWIN)
	pkg_libfile="libyices.2.dylib"
	pkg_libfileAct="libyices.dylib"
	;;
esac

