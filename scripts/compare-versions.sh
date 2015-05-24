################################################################################
#                                                                              #
# Find version differences in common packages of `ghc-pkg list` dumps.         #
#                                                                              #
# Copyright (C)  2015  Daniel Gröber <dxld@darkboxed.org>                      #
#                                                                              #
# Copying and distribution of this file, with or without modification,         #
# are permitted in any medium without royalty provided the copyright           #
# notice and this notice are preserved.  This file is offered as-is,           #
# without any warranty.                                                        #
#                                                                              #
# Usage: sh compare-versions.sh FILE1 FILE2                                    #
#                                                                              #
# Example:                                                                     #
#     sh compare-versions.sh =(ghc-pkg list) =(ssh some-host ghc-pkg list)     #
#                                                                              #
#   Where `=(command)` is equivalent to:                                       #
#     `(tmp=$(mktemp); command > $tmp; echo $tmp)`                             #
#                                                                              #
#                                                                              #
# The output consists of lines in the format:                                  #
#    <PKG> <VERSION1> <VERSION2>                                               #
# VERSION1 is the version from FILE1 and VERSION2 is the version from FILE2    #
#                                                                              #
################################################################################

t1=$(mktemp)
t2=$(mktemp)

grep "^ " "$1" | sed 's/ *\(.*\)-\([0-9.]\+\)/\1 \2/' | sort > $t1
grep "^ " "$2" | sed 's/ *\(.*\)-\([0-9.]\+\)/\1 \2/' | sort > $t2

comm -3 -2 $t1 $t2 | sort -k 1b,1 > $t1.diff
comm -3 -1 $t1 $t2 | sort -k 1b,1 > $t2.diff

join $t1.diff $t2.diff | sort | uniq
