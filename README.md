# **E-Value Re-Analysis of Many Labs 2**

Forked from [General ManyLabs2](https://github.com/ManyLabsOpenScience/ManyLabs2)

## Data

Only the `ML2_KeyTable.csv` uses the updated Google shared file.

- [MLS_S1](OSFdata/!!RawData/ML2_S1.csv)
- [MLS_S2](OSFdata/!!RawData/ML2_S2.csv)
- [ML2_SourceInfo](OSFdata/!!KeyTables/ML2_SourceInfo%20-%20ML2_SourceInfo.csv): https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/pub?gid=1435507167&single=true&output=csv
- [ML2_KeyTable](ML2_KeyTable.csv) (ML2masteRkey): https://docs.google.com/spreadsheets/d/1fqK3WHwFPMIjNVVvmxpMEjzUETftq_DmP5LzEhXxUHA/export?format=csv
```
shasum OSFdata/\!\!RawData/ML2_S1.csv
f289bf475c667c0156dd849c879654ef7d27f060  ML2_S1.csv

shasum OSFdata/\!\!RawData/ML2_S2.csv
220f1120a3b85b708dcbac477467150f7a62e60e  OSFdata/!!RawData/ML2_S2.csv

shasum OSFdata/\!\!KeyTables/ML2_SourceInfo\ -\ ML2_SourceInfo.csv
a0b06ac1611efd0598599c80554f5e94df9c173c  OSFdata/!!KeyTables/ML2_SourceInfo - ML2_SourceInfo.csv

shasum ML2_KeyTable.csv
d031123431867855ad58514e736a8dbfdd97b500  ML2_KeyTable.csv
```

To set the working directory correctly in the `.R`, see [here](01_tables/Hauser_1_global.R)

## [Table](01_tables/README.md)

## Main supplementary materials

* [General comments on DATA PREPARATION, ANALYSIS, AND REPORTING](https://docs.google.com/document/d/1beUHUZJpOl4B9E_pxI_ff8m4QitjxJFE5slwYk8HKiI/edit?ts=59a96a2a)
* [Data cleaning report](https://manylabsopenscience.github.io/ML2_data_cleaning)
    - The code is contained in the [`Rmarkdown` file](https://raw.githubusercontent.com/ManyLabsOpenScience/ManyLabsOpenScience.github.io/master/ML2_data_cleaning.Rmd) that created the report

## Files used for *R* code review:

* [To Code Review Instructions](https://ManyLabsOpenScience.github.io/ML2_RcodeReview)
* [To PoPS Proposal](https://ManyLabsOpenScience.github.io/ML2_PoPS_proposal)
* [To Data Cleaning Report](https://ManyLabsOpenScience.github.io/ML2_data_cleaning)
* [To Analysis-Specific Variable Functions](https://ManyLabsOpenScience.github.io/ML2_varfuns)

## Github repositories with **R** reproducible analyses and function libraries

* [A single-file sourceable function library]() - If you do not want to install the package
* [The manylabRs R package](https://github.com/ManyLabsOpenScience/manylabRs/tree/master/pkg)
* [Code to reproduce meta analyses and funnel plots](https://github.com/ManyLabsOpenScience/ManyLabs2/tree/master/Script%20-%20Meta%20analyses)
* [Code to reproduce figures](https://github.com/ManyLabsOpenScience/ManyLabs2/tree/master/Script%20-%20Figures)
* [Code to extract the data used to create figures and tables](https://github.com/ManyLabsOpenScience/ManyLabs2/tree/master/Script%20-%20Generate%20Figure%20and%20Table%20data)

## Lookup tables used to generate datasets and analyses

* [**masterKey**](https://docs.google.com/spreadsheets/d/1fqK3WHwFPMIjNVVvmxpMEjzUETftq_DmP5LzEhXxUHA/edit?usp=sharing) - Contains the **R** code for all analyses
* [**sourceInfo**](https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/edit?usp=sharing) - Contains information about all particpating labs (sources)
* [**Codebook for primary analyses**](https://docs.google.com/spreadsheets/d/1DK2YrekUsfCFcgxyUnIy4-5oyunne8lk7i8CcsWWw9w/edit?usp=sharing)
* [**Filename Filters**](https://docs.google.com/spreadsheets/d/1OLKcyyoYfPds5s4wRpqzXU3lACFr94Ve-cR2l_zodtU/edit?usp=sharing) - Some filenames and sourcenames had to be adjusted, this table records those adjustments.
* [**Zhong Text**](https://docs.google.com/spreadsheets/d/1J9I1JVTQqCrC7x5gz3TzA7sUCvjKJGfz0nh8EgfVAVs/edit?usp=sharing) - The different translations of the text used in the Zhong et al. study, including character count , participants had to copy more than half of the text to be included in analyses.
* [**Zavani Unique Values**](https://docs.google.com/spreadsheets/d/1aJJcCk8UvefbSAIePhmh6kEolOc5RwwB3cC_hBtrH5c/edit?usp=sharing) - Contains the unique responses given in the Zavani et al. study for all languages, including a categorisation of whether the response was correct or not.


## Shiny App for data exploration

* Local (might be slow due to limited bandwidth)
* External host

----------

## Some early tutorials (may contain info that is no longer applicable)

* [Data How To](http://fredhasselman.com/other/ML2/ML2_DataHowTo.html)
* [ML2 How To](http://fredhasselman.com/other/ML2/ML2_HowTo.html)


