cp equivs dist/build/puppetresources/ && \
    cd dist/build/puppetresources && \
    equivs-build -f equivs && \
    mv puppetresources_*_amd64.deb ../../../ && \
    rm -f puppetresources*.changes && \
    rm -f puppetresources*.dsc && \
    rm -f puppetresources*.tar.gz

