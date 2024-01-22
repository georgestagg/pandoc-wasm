FROM terrorjack/asterius

ARG BUILD=/root/_ahc/build
ARG INSTALL=/root/_ahc/install

COPY src /root/pandoc-wasm/src
COPY toml-parser /root/pandoc-wasm/toml-parser
COPY yaml /root/pandoc-wasm/yaml
COPY zip-archive /root/pandoc-wasm/zip-archive
COPY cabal.project /root/pandoc-wasm/cabal.project
COPY pandoc-wasm.cabal /root/pandoc-wasm/pandoc-wasm.cabal
COPY asterius/ghc-toolkit/boot-libs/asterius-prelude/src/Asterius /root/pandoc-wasm/src/Pandoc/Asterius

WORKDIR /root/pandoc-wasm

RUN ahc-cabal update
RUN ahc-cabal configure --allow-older=base
RUN ahc-cabal install --builddir=$BUILD --installdir=$INSTALL --only-dependencies



