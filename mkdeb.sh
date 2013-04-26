#!/bin/bash
VERSION=$1

if [ -z "$VERSION" ]
then
    echo "please set version"
    exit 4
fi

cp equivs dist/build/puppetresources/ && \
    cp ../language-puppet/ruby/calcerb.rb dist/build/puppetresources/ && \
    cd dist/build/puppetresources && \
    sed -i -e "s/Version: .*/Version: $VERSION/" equivs && \
    equivs-build -f equivs && \
    mv puppetresources_*{_amd64.deb,_amd64.changes,.dsc,.tar.gz} ../../../

