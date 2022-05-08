# Errors in CEDA
This repository compiles the yearly California Elections Data Archive (CEDA) files into one large composite file for all available years, and provides fixes for errors that were found via hand-checking done by the authors and contributors.

The script `00-ceda_compile.R` reads in all yearly spreadsheet files downloaded from the CSU Sacramento CEDA portal [here](https://csus-dspace.calstate.edu/handle/10211.3/210187), compiles them into similar format, and outputs a composite file.

The script `01-ceda_fixes.R` reads in the composite file and makes line-edits to vote totals, district identifiers, and other columns as a result of accuracy checks performed by a team of researchers and other contributors to this collective resource. This script then outputs a fixed composite dataset with the updates made.

## To contribute further edits:

Please contribute and document all edits that you believe are needed for the composite elections data file by submitting a pull request for the `01-ceda_fixes.R` script file. Alternatively, [email the authors](mailto:jdbk@hks.harvard.edu,ribernhard@ucdavis.edu?subject=CEDA%20data%20fixes), post a suggested edit in [Issues](https://github.com/justindbk/ceda/issues), shoot us a Twitter DM, or contact us via any other means of contact. We appreciate any assistance from others in keeping this collective resource accurate.
