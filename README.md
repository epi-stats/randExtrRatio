This R package generates random allocation sequences for trials with extreme 
allocation ratios. For example, the study 'Efficacy and safety of combination moxidectin and albendazole, 
ivermectin and albendazole and albendazole alone in adolescents and adults infected with Trichuris trichiura: 
a randomized controlled trial' (doi:10.1093/cid/ciad387)
had an allocation ratio of 21:21:2:2:8, resulting in a minimum block size of 54.


In this situation, simple block randomisation may cause problems due to sampling fluctuation.
In particular, arms with a small ratio value might be heavily over- or under-represented 
if the anticipated sample size is not fully reached.
One solution would be to modify the ratio slightly to achieve a smaller minimum block size (e.g. 10:10:1:1:4).
This package provides an alternative solution that keeps the original ratios.
The package borrows ideas from block randomisation, biased coin design and restricted randomisation.  

To install the package:

pak::pak("epi-stats/randExtrRatio")

or alternatively

devtools::install_github("epi-stats/randExtrRatio")
