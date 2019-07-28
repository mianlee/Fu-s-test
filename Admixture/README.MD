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

The program called [**Admixture**](http://software.genetics.ucla.edu/admixture/) needs ```.bed``` file format to run, and we also need to filter out some individuals/populations, like Chimp, Neanderthal genome, using [**Plink**](http://www.cog-genomics.org/plink2/), which also needs the ```.bed``` file format. So I have to convert the orginal ```PACKEDANCESTRYMAP``` format to ```PACKEDPED``` using ```convertf``` program in [EIGENSOFT v6.1.4](https://www.hsph.harvard.edu/alkes-price/software/) package. The ```PACKEDPED``` format using by **EIGENSOFT** pacakge is the same/similar to **Plink** ```.bed``` format.

See the ```PACKEDANCESTRYMAP``` and ```PACKEDPED``` format used by **EIGENSOFT** , [available here](https://github.com/DReichLab/EIG/tree/master/CONVERTF) and See the ```.bed``` format used by **Plink** , [available here](http://zzz.bwh.harvard.edu/plink/data.shtml#long).


Making parameter file for  ```convertf```:
```

# Convert the PACKEDANCESTRYMAP format to PACKEDPED.
# Use nano text editor in the terminal to make the par file named: " par.PACKEDANCESTRYMAP.PACKEDPED ".

nano par.PACKEDANCESTRYMAP.PACKEDPED

# And type the following in the text editor and save it.

genotypename: Haak.geno
snpname: Haak.snp
indivname: Haak.ind
outputformat: PACKEDPED
genotypeoutname: Haak.bed
snpoutname: Haak.bim
indivoutname: Haak.fam
```

Run ```convertf```:

```
/Path/To/EIG-6.1.4/bin/convertf -p par.PACKEDANCESTRYMAP.PACKEDPED 


packed geno read OK
numvalidind:   2071  maxmiss: 2071001
packedped output
##end of convertf run.
````

Now you have:

```
Haak.bed
Haak.bim
Haak.fam
```

If you check the ```data.fam``` file, I noticed that the family/population group information (the first column, showing only numbers) were lost during the format conversion. I found the population group information in the ```data.ind``` file, and updated the ```data.fam```file in excel manually. I also uploaded the modified ```data.fam``` file in the folder.
  
  ```
  # The original data.fam file.
  
  head Haak.fam
  
  1 SA1004 0 0 2 1
  2 SA063 0 0 2 1
  3 SA010 0 0 2 1
  4 SA064 0 0 1 1
  5 SA073 0 0 2 1
  6 SA1025 0 0 2 1
  7 SA078 0 0 2 1
  8 SA083 0 0 2 1
  9 Yuk_009 0 0 1 1
  10 Yuk_025 0 0 2 1

  # The updated data.fam file.
  
  head Haak.fam
  
  Khomani	SA1004	0	0	2	1
  Khomani	SA063	0	0	2	1
  Khomani	SA010	0	0	2	1
  Khomani	SA064	0	0	1	1
  Khomani	SA073	0	0	2	1
  Khomani	SA1025	0	0	2	1
  Khomani	SA078	0	0	2	1
  Khomani	SA083	0	0	2	1
  Yukagir	Yuk_009	0	0	1	1
  Yukagir	Yuk_025	0	0	2	1

  ```

[Haak.fam](data/Haak.fam)



By comapring the full and high resolution [admixture plot](data/admixture.haak.pdf) made by Haak *et al*. (2015) with the [Fully public genotype dataset (354,212 SNPs)](https://reich.hms.harvard.edu/datasets) that I downloaded, there are only **2013 individuals** (92 ancient + 1921 modern) from 193 population categories that match/overlap between the admxiture plot and the "fully public genotype dataset".

They are 41 population categories in the admixture plot but can't find in the "fully public genotype dataset".


So, I decided to just use those **2013 individuals** (92 ancient + 1921 modern) from 193 population categories to conducte admixture analysis.


**I need to extract those individuals from the "fully public genotype dataset" using ```Plink```. **


1. I made a list of those 190 population categories, [populationlist](data/population.list.txt).

2. I make a Family ID / Individual ID pairs file required by ```Plink```.

```
caat population.txt | while read line
do 
grep -w "$line" Haak.fam | awk '{print $1 "\t" $2}' >> list.ind.txt
done

```

[list.ind.txt](data/list.ind.txt)

```
head list.ind.txt

Baalberge_MN	I0559
Baalberge_MN	I0560
Baalberge_MN	I0807
Esperstedt_MN	I0172
Spain_MN	I0405
Spain_MN	I0406
Spain_MN	I0407
Spain_MN	I0408
```
3. Run the ```Plink``` to extract individuals.

```
plink --bfile Haak --keep list.ind.txt --make-bed --out admixture


# Log file
PLINK v1.90b4 64-bit (20 Mar 2017)             www.cog-genomics.org/plink/1.9/
(C) 2005-2017 Shaun Purcell, Christopher Chang   GNU General Public License v3
Logging to admixture.log.
Options in effect:
  --bfile Haak
  --keep list.ind.txt
  --make-bed
  --out admixture

8703 MB RAM detected; reserving 4351 MB for main workspace.
354212 variants loaded from .bim file.
2071 people (1307 males, 764 females) loaded from .fam.
2071 phenotype values loaded from .fam.
--keep: 2013 people remaining.
Using 1 thread (no multithreaded calculations invoked).
Before main variant filters, 2013 founders and 0 nonfounders present.
Calculating allele frequencies... done.
Total genotyping rate in remaining samples is 0.979204.
354212 variants and 2013 people pass filters and QC.
Among remaining phenotypes, 0 are cases and 2013 are controls.
--make-bed to admixture.bed + admixture.bim + admixture.fam ... done.

#sort the oder of those individuals

plink --bfile admixture --indiv-sort f list.ind.txt --make-bed --out Haak.admixture



# Log file
PLINK v1.90b4 64-bit (20 Mar 2017)             www.cog-genomics.org/plink/1.9/
(C) 2005-2017 Shaun Purcell, Christopher Chang   GNU General Public License v3
Logging to Haak.admixture.log.
Options in effect:
  --bfile admixture
  --indiv-sort f list.ind.txt
  --make-bed
  --out Haak.admixture

8703 MB RAM detected; reserving 4351 MB for main workspace.
354212 variants loaded from .bim file.
2013 people (1272 males, 741 females) loaded from .fam.
2013 phenotype values loaded from .fam.
Using 1 thread (no multithreaded calculations invoked).
Before main variant filters, 2013 founders and 0 nonfounders present.
Calculating allele frequencies... done.
Total genotyping rate is 0.979204.
354212 variants and 2013 people pass filters and QC.
Among remaining phenotypes, 0 are cases and 2013 are controls.
--make-bed to Haak.admixture.bed + Haak.admixture.bim + Haak.admixture.fam ...
done.

```


So now, the data set contains 1,921 modern humans and 92 ancient ones, with the oder consistent with the Haak's admixture plot.


```
Haak.admixture.bed
Haak.admixture.bim
Haak.admixture.fam
```



The initial set of 354,212 SNPs was pruned for linkage disequilibrium in PLINK (v. 1.9) using parameters: 

```--indep-pairwise 200 25 0.5```

The command above that specifies 200 25 0.5 would a) consider a window of 200 SNPs, b) calculate LD between each pair of SNPs in the window, b) remove one of a pair of SNPs if the LD is greater than 0.5, c) shift the window 25 SNPs forward and repeat the procedure.

```
plink --bfile Haak.admixture --indep-pairwise 200 25 0.5 


# log file
PLINK v1.90b4 64-bit (20 Mar 2017)             www.cog-genomics.org/plink/1.9/
(C) 2005-2017 Shaun Purcell, Christopher Chang   GNU General Public License v3
Logging to plink.log.
Options in effect:
  --bfile Haak.admixture
  --indep-pairwise 200 25 0.5

8703 MB RAM detected; reserving 4351 MB for main workspace.
354212 variants loaded from .bim file.
2013 people (1272 males, 741 females) loaded from .fam.
2013 phenotype values loaded from .fam.
Using 1 thread (no multithreaded calculations invoked).
Before main variant filters, 2013 founders and 0 nonfounders present.
Calculating allele frequencies... done.
Total genotyping rate is 0.979204.
354212 variants and 2013 people pass filters and QC.
Among remaining phenotypes, 0 are cases and 2013 are controls.
Pruned 11404 variants from chromosome 1, leaving 18583.
Pruned 12049 variants from chromosome 2, leaving 19435.
Pruned 10022 variants from chromosome 3, leaving 15867.
Pruned 8461 variants from chromosome 4, leaving 13896.
Pruned 8502 variants from chromosome 5, leaving 13956.
Pruned 8591 variants from chromosome 6, leaving 13025.
Pruned 6642 variants from chromosome 7, leaving 11045.
Pruned 7128 variants from chromosome 8, leaving 11341.
Pruned 5327 variants from chromosome 9, leaving 9667.
Pruned 7526 variants from chromosome 10, leaving 12145.
Pruned 7035 variants from chromosome 11, leaving 11260.
Pruned 6699 variants from chromosome 12, leaving 11259.
Pruned 4093 variants from chromosome 13, leaving 7907.
Pruned 4476 variants from chromosome 14, leaving 7685.
Pruned 4229 variants from chromosome 15, leaving 7404.
Pruned 3944 variants from chromosome 16, leaving 7647.
Pruned 3144 variants from chromosome 17, leaving 6349.
Pruned 4039 variants from chromosome 18, leaving 7191.
Pruned 1577 variants from chromosome 19, leaving 3775.
Pruned 3667 variants from chromosome 20, leaving 6316.
Pruned 1651 variants from chromosome 21, leaving 3393.
Pruned 1537 variants from chromosome 22, leaving 3323.
Pruning complete.  131743 of 354212 variants removed.
Marker lists written to plink.prune.in and plink.prune.out .
```

You are getting two files [plink.prune.in](data/plink.prune.in) and [plink.prune.out](data/plink.prune.out). To make a new, pruned file, then use the following command to extract SNPs that meet the LD standard.

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




