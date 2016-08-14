#!/bin/sh
hexdump -v \
        -e '/1  "%_ad#    "' \
        -e '/1 " = %02x hex "' \
        -e '/1 " = %03u dec\n"' \
        $*
