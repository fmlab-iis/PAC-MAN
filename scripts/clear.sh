#!/bin/bash
 find . -maxdepth 1 \
         -not -name "*.sh" | xargs rm
#        -not -name "*call.c" \
#        -not -name "*termination.c" \
#        -not -name "*ground.c" \
rm -rf output
