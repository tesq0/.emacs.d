export PATH=$coreutils/bin

set -- $deps
while [ $# -gt 0 ]; do
    package=$1
    echo "COPYING $package"
    mkdir -p $out/$package
    src=$2
    cp -r $src/* $out/$package
    shift 2
done

