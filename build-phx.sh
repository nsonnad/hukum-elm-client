#!/bin/sh

elm-app build && cp -r ./build/* ../hukum_sockets/priv/static
