#!/bin/bash

cd ./scripts/
Rscript run_all.r
cd ..
rm -r ./data/cache/
rm -r ./figure/

Rscript -e "knitr::knit2pdf('main.rnw')"
zathura main.pdf

# remove some unneeded bib stuff
sed '/^  url/ d' kbib.bib
sed '/^  note/ d' kbib.bib
sed '/^  file/ d' kbib.bib
