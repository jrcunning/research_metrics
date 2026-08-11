# SciVal Policy Impact Exports

This folder holds the CSV exports from the SciVal Policy Impact section that
are used in the report for the Haerther Center publication DOI set.

The current report expects only these files:

- `Summary_Metrics*.csv`
- `Policies.csv`

## Quarterly Refresh Process

This is a partly manual workflow because SciVal needs a publication set built
from the current DOI list.

1. Update the local publication references first, including any manually curated
   DOIs in the source workbooks or `data/pre2019_publications.txt`.
2. Knit `R/metrics.Rmd` once to regenerate:
   - `output/publication_dois.txt`
   - `output/publication_doi_details.csv`
3. In SciVal, create or update a publication set/entity using the DOIs from
   `output/publication_dois.txt`.
4. In the SciVal Policy Impact section for that DOI-based publication set,
   download the summary metrics CSV and the citing policy document list as
   `Policies.csv`.
5. Place those two exported CSVs in this folder. Remove or archive older
   matching summary exports if needed; the report uses the most recently
   modified `Summary_Metrics*.csv` file.
6. Knit `R/metrics.Rmd` again so the Policy Impact section reflects the new
   SciVal exports.

The report uses the summary file's SciVal metadata rows for date ranges, data
update dates, and export dates. The DOI detail file is useful for auditing
which publications were included in the SciVal upload and which records still
lack a DOI.
