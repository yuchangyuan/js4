#!/bin/sh

emcc \
    -Os \
    --memory-init-file 0 \
    -s MODULARIZE=1 \
    -s EXPORTED_FUNCTIONS='["_hs_cryptohash_sha512_init","_hs_cryptohash_sha512_update", "_hs_cryptohash_sha512_finalize"]' \
    -s EXTRA_EXPORTED_RUNTIME_METHODS='["setValue", "getValue"]' \
    -s WASM=0 \
    -o sha512.js \
    ref/sha512.c
