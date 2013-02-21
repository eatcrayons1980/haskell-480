#!/bin/bash

ghc-pkg list | grep parsec
if [ $? -eq 1 ]
then
    echo "Parsec is not available. Attempting to build from source..."
    tar -xzf parsec-3.1.3.tar.gz
    cd parsec-3.1.3
    runhaskell Setup configure --user --prefix=$HOME
    runhaskell Setup build
    runhaskell Setup install
    cd ..
fi
