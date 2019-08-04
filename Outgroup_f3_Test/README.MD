## Preparing for the Dateset

The dataset I used here is the same dataset used to do the [Admxiture Analysis](https://github.com/mianlee/Fu-s-test/tree/master/Admixture).The dataset contains 2013 individuals (92 ancient + 1921 modern) from 193 population categories, each represented by 354,212 autosomal single nucleotide polymorphisms (SNPs), see the [**```.ind```**](data/Haak_2013.ind) file. This dataset is originally from [Haak *et al*. Nature (2015)](https://www.nature.com/articles/nature14317), [available here](https://reich.hms.harvard.edu/datasets).

1. Because I used ```Plink``` to filter out some individuals, so I will start with the ```Plink``` file format after filteration:

```
Haak.admxi.final.bed
Haak.admxi.final.bim
Haak.admxi.final.fam
```
If you want to see how I did this, please refer to the [Admxiture section](https://github.com/mianlee/Fu-s-test/tree/master/Admixture).

2. Converting ```.bed``` format to ```packedancestrymap``` format using ```convertf``` program in [ADMIXTOOLS](https://github.com/DReichLab/AdmixTools) package.

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

4. Fixing the ```*.ind``` file.

```
awk -F ":" '{print $1 "\t" $2}' Haak_2013.ind | awk '{print $1":"$2 "\t" $3 "\t" $1}' > fixed.ind

mv fixed.ind Haak_2013.ind
```

When you are converting ```.bed``` format to ```packedancestrymap```format, you need to change **'Control'** to **'Population/group information'** in the third column of ```.ind``` file. 

See the difference between the **old** [```Haak_2013.ind```](data/Old_Haak_2013.ind) and the **new** [```Haak_2013.ind```](data/Haak_2013.ind).

5. Now you have three files to work with.

```
Haak_2013.geno
Haak_2013.snp
Haak_2013.ind
```
6. Making a population list file.

```
awk '{print$3}' Haak_2013.ind | sort | uniq > pop.list.txt
```

Here is the population list file: [pop.list.txt](data/pop.list.txt). 193 population categories: 33 ancient + 163 modern.


## Running Outgroup_f3_Test

For the first test, I am going to show how I did it using both [ADMIXTOOLS](https://github.com/DReichLab/AdmixTools) package and an R package [**```admixr```**](https://github.com/bodkan/admixr).

**f3(Yoruba; X, Y)**

### Western European hunter-gatherers

**Figure S7.3** in [Haak *et al*. (2015)](https://www.nature.com/articles/nature14317).

Three Western European hunter-gatherers (WHG):

**Loschbour** (n=1) [Iosif Lazaridis *et al*. (2014)](https://www.nature.com/articles/nature13673), **LaBrana1** (n=1) [Iñigo Olalde *et al*.(2014)](https://www.nature.com/articles/nature12960), **HungaryGamba_HG** or KO1 (n=1) [Cristina Gamba *et al*. (2014)](https://www.nature.com/articles/ncomms6257#t1).


1. ADMIXTOOLS package

In oder to run ```qp3Pop```, you neeed three parameter files ```.par``` and three ```popfilename``` files for each WHG.

```
nano par.f3.Loschbour


genotypename: Haak_2013.geno
snpname: Haak_2013.snp
indivname: Haak_2013.geno
popfilename: Loschbour.txt
```


```
nano par.f3.LaBrana


genotypename: Haak_2013.geno
snpname: Haak_2013.snp
indivname: Haak_2013.geno
popfilename: LaBrana.txt
```


```
nano par.f3.HungaryGamba_HG


genotypename: Haak_2013.geno
snpname: Haak_2013.snp
indivname: Haak_2013.geno
popfilename: HungaryGamba_HG.txt
```



### Reference:

1. Haak, Wolfgang, et al. "Massive migration from the steppe was a source for Indo-European languages in Europe." Nature 522.7555 (2015): 207.

2. Lazaridis, Iosif, et al. "Ancient human genomes suggest three ancestral populations for present-day Europeans." Nature 513.7518 (2014): 409.

3. Gamba, Cristina, et al. "Genome flux and stasis in a five millennium transect of European prehistory." Nature communications 5 (2014): 5257.

4. Olalde, Inigo, et al. "Derived immune and ancestral pigmentation alleles in a 7,000-year-old Mesolithic European." Nature 507.7491 (2014): 225.

