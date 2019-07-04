## ADMIXTURE analysis

The authors carried out admixture analysis using ADMIXTURE1 (v. 1.23), including 2,437 individuals (2,345 modern humans and 92 ancient ones).

The dataset that I will work with is the [Fully public genotype dataset (354,212 SNPs)](https://reich.hms.harvard.edu/datasets) provided by [Haak et al. Nature 2015](https://www.nature.com/articles/nature14317)

The dataset contains **2,071** individuals, each represented by **354,212 autosomal** single nucleotide polymorphisms (SNPs).The full dataset of 2,437 individuals includes samples that cannot be posted in public.

The authors excluded individuals I0114 and I0411 that appear to be related to individuals I0117 and I0410 respectively, as in their experience, unsupervised ADMIXTURE clustering analysis often infers spurious clusters when pairs of relatives are included in the analysis dataset. 

I also excluded Denisovan and Neandertal individuals, see [Exclude_list.txt](Exclude_list.txt).

```
plink 1.9
```

```
# Remove 6 individuals listed in the 'Exclude_list.txt'
plink --bfile data --remove Exclude_list.txt --make-bed --out admixture


# Log file
PLINK v1.90b4 64-bit (20 Mar 2017)             www.cog-genomics.org/plink/1.9/
(C) 2005-2017 Shaun Purcell, Christopher Chang   GNU General Public License v3
Logging to admixture.log.
Options in effect:
  --bfile data
  --make-bed
  --out admixture
  --remove Exclude_list.txt

8703 MB RAM detected; reserving 4351 MB for main workspace.
354212 variants loaded from .bim file.
2071 people (1307 males, 764 females) loaded from .fam.
2071 phenotype values loaded from .fam.
--remove: 2065 people remaining.
Using 1 thread (no multithreaded calculations invoked).
Before main variant filters, 2065 founders and 0 nonfounders present.
Calculating allele frequencies... done.
Total genotyping rate in remaining samples is 0.973221.
354212 variants and 2065 people pass filters and QC.
Among remaining phenotypes, 0 are cases and 2065 are controls.
--make-bed to admixture.bed + admixture.bim + admixture.fam ... done.
```

So now, the data set contains 1,973 modern humans and 92 ancient ones.



The initial set of 354,212 SNPs was pruned for linkage disequilibrium in PLINK (v. 1.9) using parameters ```--indep-pairwise 200 25 0.5```









