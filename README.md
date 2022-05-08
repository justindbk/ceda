# CEDA
This repository compiles the yearly California Elections Data Archive (CEDA) files into one large composite file for all available years, and provides fixes for accuracy that resulted from hand-checking done by the authors and contributors.

The script <00-ceda_compile.R> reads in all yearly files, compiles them into similar format, and outputs a composite file.

The script <01-ceda_fixes.R> reads in the composite file and makes line-edits to vote totals, district identifiers, and other columns as a result of accuracy checks performed by a team of researchers and other contributors to this collective resource. This script then outputs a fixed composite dataset with the updates made.

*To contribute further edits*
Please contribute and document all edits that you believe are needed for the elections data by submitting a pull request for the <01-ceda_fixes.R> code file.
