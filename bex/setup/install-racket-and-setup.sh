#!/bin/bash

function help(){
	echo "First argument must be project root (usually the parent of `contracts-and-testing`), and second must be the setup config (one of `contracts-and-testing/bex/setup/*-setup-config.rkt`)"
	exit 1
}

case "$1" in
    ""|"-h"|"--help"|"h"|"help" )
	help
	;;
    * )
	;;
esac

if [ "$2" == "" ]; then
    help
fi

pushd "$1"

printf "Installing Racket if necessary\n\n\n"
VERSION="8.9"
if [ -d "./racket" ]; then
    echo "$(pwd)/racket already exists; skipping installing racket"
else
    INSTALLER="racket-${VERSION}-x86_64-linux-cs.sh"
    wget "https://mirror.racket-lang.org/installers/${VERSION}/$INSTALLER"
    chmod u+x ./$INSTALLER
    # If you are using mac with Apple Silicon chip,
    # INSTALLER should end with -aarch64-macosx-cs.dmg instead of -x86_64-linux-cs.sh.
    # If you are using mac, click on the dmg file yourself and move the Racket 8.9 folder into the parent directory
    # of contracts-and-testing instead of into Applications. Then, rename "Racket 8.9" to "racket"
    # and re-run this script.
    ./$INSTALLER <<EOF
no
4

EOF
fi

./racket/bin/raco pkg install --auto "https://github.com/LLazarek/rscript.git"

find contracts-and-testing/ -name compiled -type d -prune -exec rm -r '{}' ';'

printf "Running setup script\n\n\n"
./racket/bin/racket contracts-and-testing/bex/setup/setup.rkt -c "$2"

popd
