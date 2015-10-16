#!/bin/bash
 find . -maxdepth 1 \
         -not -name "*.sh" -not -name "*.md" | xargs rm
#        -not -name "*call.c" \
#        -not -name "*termination.c" \
#        -not -name "*ground.c" \
rm -rf output
rm -rf sideProducts
