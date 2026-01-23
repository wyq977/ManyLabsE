This project reproduces the meta-analysis of Many Labs 2 SAVI.

## Naming
- R code should follow the tidyverse style guide.
- `*.R` files should use snake_case naming (e.g., `file_name.R`).
- Use 2-space indentation and limit lines to approximately 79 characters.


## File management
- `manylabRs_SOURCE.R` is the original source script, which is too long and difficult to review.
- The `OSFdata` directory should contain the unmodified data, including the key data, source information table, and raw data from the two slates.
	- `ML2_KeyTable.csv`
	- `ML2_SourceInfo.csv`
	- `ML2_S1.csv` (Slate 1)
	- `ML2_S2.csv` (Slate 2)
- Analyses should be grouped into three folders: `tTest`, `zTest`, and `table`.
- Within each analysis folder, there should be subfolders for each study, named using the study's description (e.g., `Trolley Dilemma 1 (Hauser et al., 2007)`).
- For now, do not create any further sub-categorizations within each study's folder.

## Within each analysis
- There should be a `.R` script to run the analysis.


## Commit & Pull Request Guidelines
- Use short, imperative commit titles (e.g., `Add ...`, `Fix ...`, `Remove ...`).
- Keep commits focused on a single logical change.
- In your pull request, describe the problem, your approach, and any commands you ran (e.g., for testing, linting, or documentation).
- Link to relevant issues when applicable.
- For changes to figures, include brief notes or thumbnails.
- Avoid committing large data files or caches.