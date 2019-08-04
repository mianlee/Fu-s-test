## Preparing for the Dateset

The dataset I used here is the same dataset used to do the [admxiture analysis](https://github.com/mianlee/Fu-s-test/tree/master/Admixture).The dataset contains 2013 individuals (92 ancient + 1921 modern) from 193 population categories, each represented by 354,212 autosomal single nucleotide polymorphisms (SNPs), see the [```.ind``` file](data/). This dataset is originally from [Haak *et al*. Nature (2015)](https://www.nature.com/articles/nature14317), [available here](https://reich.hms.harvard.edu/datasets).

Because I need to use ```Plink``` to filter out some individuals, so I will start with the ```Plink``` file format after filteration:

```
Haak.admxi.final.bed
Haak.admxi.final.bim
Haak.admxi.final.fam
```

Converting ```.bed``` format to ```packedancestrymap``` using ```convertf``` program in [Admixtools](https://github.com/DReichLab/AdmixTools)

```
 nano par.packedped.PACKEDANCESTRYMAP 
 
 
```
