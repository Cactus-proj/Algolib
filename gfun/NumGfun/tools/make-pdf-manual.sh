#!/bin/bash

FILES="NumGfun.mws  bound_diffeq.mws  bound_diffeq_tail.mws \
    bound_ratpoly.mws  bound_rec.mws  diffeqtoproc.mws \
    evaldiffeq.mws"
DEST=NumGfun/help/NumGfunUserManual.pdf

ls NumGfun/help || exit 1
ls NumGfun/help/*.pdf 2>&1 && exit 2

for f in $FILES; do
    echo Processing $f...
    cnee --replay --file NumGfun/tools/maple-export-to-pdf.xnee --time 10 & \
        maple -x "NumGfun/help/$f"
done

(cd NumGfun/help && pdflatex pdf-manual-title-page.tex)

(for f in $FILES; do
    echo "NumGfun/help/${f%.mws}.pdf"
done; echo cat output $DEST) | xargs pdftk NumGfun/help/pdf-manual-title-page.pdf
