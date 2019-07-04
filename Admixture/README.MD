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



The initial set of 354,212 SNPs was pruned for linkage disequilibrium in PLINK (v. 1.9) using parameters: 

```--indep-pairwise 200 25 0.5```

The command above that specifies 200 25 0.5 would a) consider a window of 200 SNPs, b) calculate LD between each pair of SNPs in the window, b) remove one of a pair of SNPs if the LD is greater than 0.5, c) shift the window 25 SNPs forward and repeat the procedure.

```
plink --bfile admixture --indep-pairwise 200 25 0.5 


# log file

PLINK v1.90b4 64-bit (20 Mar 2017)             www.cog-genomics.org/plink/1.9/
(C) 2005-2017 Shaun Purcell, Christopher Chang   GNU General Public License v3
Logging to plink.log.
Options in effect:
  --bfile admixture
  --indep-pairwise 200 25 0.5

8703 MB RAM detected; reserving 4351 MB for main workspace.
354212 variants loaded from .bim file.
2065 people (1305 males, 760 females) loaded from .fam.
2065 phenotype values loaded from .fam.
Using 1 thread (no multithreaded calculations invoked).
Before main variant filters, 2065 founders and 0 nonfounders present.
Calculating allele frequencies... done.
Total genotyping rate is 0.973221.
354212 variants and 2065 people pass filters and QC.
Among remaining phenotypes, 0 are cases and 2065 are controls.
Pruned 11123 variants from chromosome 1, leaving 18864.
Pruned 11747 variants from chromosome 2, leaving 19737.
Pruned 9740 variants from chromosome 3, leaving 16149.
Pruned 8278 variants from chromosome 4, leaving 14079.
Pruned 8329 variants from chromosome 5, leaving 14129.
Pruned 8390 variants from chromosome 6, leaving 13226.
Pruned 6474 variants from chromosome 7, leaving 11213.
Pruned 6922 variants from chromosome 8, leaving 11547.
Pruned 5187 variants from chromosome 9, leaving 9807.
Pruned 7348 variants from chromosome 10, leaving 12323.
Pruned 6874 variants from chromosome 11, leaving 11421.
Pruned 6547 variants from chromosome 12, leaving 11411.
Pruned 4008 variants from chromosome 13, leaving 7992.
Pruned 4375 variants from chromosome 14, leaving 7786.
Pruned 4118 variants from chromosome 15, leaving 7515.
Pruned 3875 variants from chromosome 16, leaving 7716.
Pruned 3081 variants from chromosome 17, leaving 6412.
Pruned 3935 variants from chromosome 18, leaving 7295.
Pruned 1552 variants from chromosome 19, leaving 3800.
Pruned 3567 variants from chromosome 20, leaving 6416.
Pruned 1609 variants from chromosome 21, leaving 3435.
Pruned 1495 variants from chromosome 22, leaving 3365.
Pruning complete.  128574 of 354212 variants removed.
Marker lists written to plink.prune.in and plink.prune.out .
```

You are getting two files [plink.prune.in](plink.prune.in) and [plink.prune.out](plink.prune.out). To make a new, pruned file, then use the following command to extract SNPs that meet the LD standard.

```
plink --bfile admixture --extract plink.prune.in --make-bed --out admixture_prune


# Log file

PLINK v1.90b4 64-bit (20 Mar 2017)             www.cog-genomics.org/plink/1.9/
(C) 2005-2017 Shaun Purcell, Christopher Chang   GNU General Public License v3
Logging to admixture_prune.log.
Options in effect:
  --bfile admixture
  --extract plink.prune.in
  --make-bed
  --out admixture_prune

8703 MB RAM detected; reserving 4351 MB for main workspace.
354212 variants loaded from .bim file.
2065 people (1305 males, 760 females) loaded from .fam.
2065 phenotype values loaded from .fam.
--extract: 225638 variants remaining.
Using 1 thread (no multithreaded calculations invoked).
Before main variant filters, 2065 founders and 0 nonfounders present.
Calculating allele frequencies... done.
Total genotyping rate is 0.973292.
225638 variants and 2065 people pass filters and QC.
Among remaining phenotypes, 0 are cases and 2065 are controls.
--make-bed to admixture_prune.bed + admixture_prune.bim + admixture_prune.fam
... done.

```

Thus, resulting in a pruned set of 225,638 SNPs used for admxiture analysis. 




