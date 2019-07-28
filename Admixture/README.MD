## ADMIXTURE analysis

The authors carried out admixture analysis using ADMIXTURE1 (v. 1.23), including 2,437 individuals (2,345 modern humans and 92 ancient ones).

The dataset that I will work with is the [Fully public genotype dataset (354,212 SNPs)](https://reich.hms.harvard.edu/datasets) provided by [Haak *et al*. Nature (2015)](https://www.nature.com/articles/nature14317)

The dataset contains **2,071** individuals, each represented by **354,212 autosomal** single nucleotide polymorphisms (SNPs).The full dataset of 2,437 individuals includes samples that cannot be posted in public.

The authors excluded individuals I0114 and I0411 that appear to be related to individuals I0117 and I0410 respectively, as in their experience, unsupervised ADMIXTURE clustering analysis often infers spurious clusters when pairs of relatives are included in the analysis dataset. 


I downloaded the Haak *et al*. 2015 dataset from [here](https://reich.hms.harvard.edu/datasets), and then unzipped the file in the terminal:

```
tar -xvzf Haak2015PublicData.tar.gz

# The followings will show on your screen.
data.geno
data.ind
data.pops
data.snp
README
```

Then, I changed the prefix of those files from 'data' to 'Haak'.

```
Haak.geno
Haak.ind
Haak.snp
```

The program called [```Admixture```](http://software.genetics.ucla.edu/admixture/) needs ```.bed``` file format to run, and we also need to filter out some individuals/populations, like Chimp, Neanderthal genome, using [```Plink```](http://www.cog-genomics.org/plink2/), which also needs the ```.bed``` file format. So we need to convert the orginal ```PACKEDANCESTRYMAP``` format to ```PACKEDPED``` using ```convertf``` program in [EIGENSOFT v7.2.1](https://github.com/DReichLab/EIG) package. The ```PACKEDPED``` format using by **EIGENSOFT** pacakge is the same/similar to **Plink** ```.bed``` format.



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
--remove: 2064 people remaining.
Using 1 thread (no multithreaded calculations invoked).
Before main variant filters, 2064 founders and 0 nonfounders present.
Calculating allele frequencies... done.
Total genotyping rate in remaining samples is 0.973208.
354212 variants and 2064 people pass filters and QC.
Among remaining phenotypes, 0 are cases and 2064 are controls.
--make-bed to admixture.bed + admixture.bim + admixture.fam ... done.e.
```

So now, the data set contains 1,972 modern humans and 92 ancient ones.



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
2064 people (1304 males, 760 females) loaded from .fam.
2064 phenotype values loaded from .fam.
Using 1 thread (no multithreaded calculations invoked).
Before main variant filters, 2064 founders and 0 nonfounders present.
Calculating allele frequencies... done.
Total genotyping rate is 0.973208.
354212 variants and 2064 people pass filters and QC.
Among remaining phenotypes, 0 are cases and 2064 are controls.
Pruned 11148 variants from chromosome 1, leaving 18839.
Pruned 11773 variants from chromosome 2, leaving 19711.
Pruned 9764 variants from chromosome 3, leaving 16125.
Pruned 8300 variants from chromosome 4, leaving 14057.
Pruned 8346 variants from chromosome 5, leaving 14112.
Pruned 8413 variants from chromosome 6, leaving 13203.
Pruned 6491 variants from chromosome 7, leaving 11196.
Pruned 6946 variants from chromosome 8, leaving 11523.
Pruned 5200 variants from chromosome 9, leaving 9794.
Pruned 7358 variants from chromosome 10, leaving 12313.
Pruned 6889 variants from chromosome 11, leaving 11406.
Pruned 6559 variants from chromosome 12, leaving 11399.
Pruned 4015 variants from chromosome 13, leaving 7985.
Pruned 4380 variants from chromosome 14, leaving 7781.
Pruned 4129 variants from chromosome 15, leaving 7504.
Pruned 3886 variants from chromosome 16, leaving 7705.
Pruned 3089 variants from chromosome 17, leaving 6404.
Pruned 3941 variants from chromosome 18, leaving 7289.
Pruned 1561 variants from chromosome 19, leaving 3791.
Pruned 3577 variants from chromosome 20, leaving 6406.
Pruned 1612 variants from chromosome 21, leaving 3432.
Pruned 1503 variants from chromosome 22, leaving 3357.
Pruning complete.  128880 of 354212 variants removed.
Marker lists written to plink.prune.in and plink.prune.out.
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
2064 people (1304 males, 760 females) loaded from .fam.
2064 phenotype values loaded from .fam.
--extract: 225332 variants remaining.
Using 1 thread (no multithreaded calculations invoked).
Before main variant filters, 2064 founders and 0 nonfounders present.
Calculating allele frequencies... done.
Total genotyping rate is 0.973276.
225332 variants and 2064 people pass filters and QC.
Among remaining phenotypes, 0 are cases and 2064 are controls.
--make-bed to admixture_prune.bed + admixture_prune.bim + admixture_prune.fam
... done.

```

Thus, resulting in a pruned set of 225,332 SNPs used for admxiture analysis. 




