## Preparing for the Dateset

The dataset I used here is the same dataset used to do the [admxiture analysis](https://github.com/mianlee/Fu-s-test/tree/master/Admixture).The dataset contains 2013 individuals (92 ancient + 1921 modern) from 193 population categories, each represented by 354,212 autosomal single nucleotide polymorphisms (SNPs), see the [```.ind``` file](data/). This dataset is originally from [Haak *et al*. Nature (2015)](https://www.nature.com/articles/nature14317), [available here](https://reich.hms.harvard.edu/datasets).

1. Because I need to use ```Plink``` to filter out some individuals, so I will start with the ```Plink``` file format after filteration:

```
Haak.admxi.final.bed
Haak.admxi.final.bim
Haak.admxi.final.fam
```

2. Converting ```.bed``` format to ```packedancestrymap``` using ```convertf``` program in [ADMIXTOOLS](https://github.com/DReichLab/AdmixTools) package.

```
# Using nano in the terminal creat a par file named par.packedped.PACKEDANCESTRYMAP. 
nano par.packedped.PACKEDANCESTRYMAP 
 

#Type the follwing:

genotypename: Haak.admxi.final.bed
snpname: Haak.admxi.final.bim
indivname: Haak.admxi.final.fam
outputformat: PACKEDANCESTRYMAP
genotypeoutname: Haak_2013.geno
snpoutname: Haak_2013.snp
indivoutname: Haak_2013.ind
 
```
3. Running ```convertf``` program.

```
convertf -p par.packedped.PACKEDANCESTRYMAP 

# log.out
parameter file: par.packedped.PACKEDANCESTRYMAP
genotypename: Haak.admxi.final.bed
snpname: Haak.admxi.final.bim
indivname: Haak.admxi.final.fam
outputformat: PACKEDANCESTRYMAP
genotypeoutname: Haak_2013.geno
snpoutname: Haak_2013.snp
indivoutname: Haak_2013.ind
genotype file processed
numvalidind:   2013  maxmiss: 2013001
packedancestrymap output
##end of convertf run
```

4. Fixing the ```*.ind``` file:




