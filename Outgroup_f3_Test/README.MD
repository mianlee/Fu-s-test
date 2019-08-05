# Preparing for the Dateset

The dataset I used here is the same dataset used to do the [Admxiture Analysis](https://github.com/mianlee/Fu-s-test/tree/master/Admixture).The dataset contains 2013 individuals (92 ancient + 1921 modern) from 193 population categories, each represented by 354,212 autosomal single nucleotide polymorphisms (SNPs), see the [**```.ind```**](data/Haak_2013.ind) file. This dataset is originally from [Haak *et al*. Nature (2015)](https://www.nature.com/articles/nature14317), [available here](https://reich.hms.harvard.edu/datasets).

1. Because I used ```Plink``` to filter out some individuals, so I will start with the ```Plink``` file format after filteration:

```
Haak.admxi.bed
Haak.admxi.bim
Haak.admxi.fam
```
If you want to see how I did this, please refer to the [Admxiture section](https://github.com/mianlee/Fu-s-test/tree/master/Admixture).

2. Converting ```.bed``` format to ```packedancestrymap``` format using ```convertf``` program in [ADMIXTOOLS](https://github.com/DReichLab/AdmixTools) package.

```
# Using nano in the terminal creat a par file named par.packedped.PACKEDANCESTRYMAP. 
nano par.packedped.PACKEDANCESTRYMAP 
 

#Type the follwing:

genotypename: Haak.admxi.bed
snpname: Haak.admxi.bim
indivname: Haak.admxi.fam
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
genotypename: Haak.admxi.bed
snpname: Haak.admxi.bim
indivname: Haak.admxi.fam
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


# Running Outgroup_f3_Test

For the first test, I am going to show how I did it using both [ADMIXTOOLS](https://github.com/DReichLab/AdmixTools) package and an R package [**```admixr```**](https://github.com/bodkan/admixr).

**f3(Yoruba; X, Y)**

## Western European hunter-gatherers

**Figure S7.3** in [Haak *et al*. (2015)](https://www.nature.com/articles/nature14317).

Three Western European hunter-gatherers (WHG):

**Loschbour** (n=1) [Iosif Lazaridis *et al*. (2014)](https://www.nature.com/articles/nature13673), **LaBrana1** (n=1) [Iñigo Olalde *et al*.(2014)](https://www.nature.com/articles/nature12960), **HungaryGamba_HG** or KO1 (n=1) [Cristina Gamba *et al*. (2014)](https://www.nature.com/articles/ncomms6257#t1).


### 1. ADMIXTOOLS Package

**In oder to run ```qp3Pop```, you neeed three parameter files ```.par``` and three ```popfilename``` files for each WHG.**

```
nano par.f3.Loschbour


genotypename: Haak_2013.geno
snpname: Haak_2013.snp
indivname: Haak_2013.ind
popfilename: Loschbour.txt
```


```
nano par.f3.LaBrana


genotypename: Haak_2013.geno
snpname: Haak_2013.snp
indivname: Haak_2013.ind
popfilename: LaBrana.txt
```


```
nano par.f3.HungaryGamba_HG


genotypename: Haak_2013.geno
snpname: Haak_2013.snp
indivname: Haak_2013.ind
popfilename: HungaryGamba_HG.txt
```

```
awk '{print "Loschbour" "\t" $1 "\t" "Yoruba"}' pop.list.txt > Loschbour.txt
```

```
awk '{print "LaBrana1" "\t" $1 "\t" "Yoruba"}' pop.list.txt > LaBrana.txt
```

```
awk '{print "HungaryGamba_HG" "\t" $1 "\t" "Yoruba"}' pop.list.txt > HungaryGamba_HG.txt
```

**Running ```qp3Pop```:**

```
qp3Pop -p par.f3.Loschbour > Loschbour.f3.results

qp3Pop -p par.f3.LaBrana > LaBrana.f3.results

qp3Pop -p par.f3.HungaryGamba_HG > HungaryGamba_HG.f3.results
```
**Then you will have three files:**

[Loschbour.f3.results](data/Loschbour.f3.results), [LaBrana.f3.results](data/LaBrana.f3.results) and [HungaryGamba_HG.f3.results](data/HungaryGamba_HG.f3.results).

When you check the ```*.f3.results``` files, as you see, we don’t want comment/log rows in the beginning and columns 1. You can use ```awk``` in the terminal to keep only columns 2, 3, 4, 5, 6, 7, 8:

```
grep "result:" Loschbour.f3.results | awk '{print$2 "\t" $3 "\t" $4 "\t" $5"\t" $6 "\t" $7 "\t" $8}' > Loschbour.f3

grep "result:" LaBrana.f3.results | awk '{print$2 "\t" $3 "\t" $4 "\t" $5"\t" $6 "\t" $7 "\t" $8}' > LaBrana.f3

grep "result:" HungaryGamba_HG.f3.results | awk '{print$2 "\t" $3 "\t" $4 "\t" $5"\t" $6 "\t" $7 "\t" $8}' > HungaryGamba_HG.f3
```

[Loschbour.f3](data/Loschbour.f3), [LaBrana.f3](data/LaBrana.f3) and [HungaryGamba_HG.f3](data/HungaryGamba_HG.f3).

**Plot the f3 results in R:**

There is no need to plot all comparision, so I only plot the first 30 after sorting from highest f3 value to the lowest.


```
library(ggplot2)

Loschbourf3data <- read.table("Loschbour.f3", col.names=c("PopA", "PopB", "Outgroup", "F3", "StdErr", "Z", "SNPs"))

attach(Loschbourf3data)

Loschbour <- Loschbourf3data[PopA == "Loschbour", ]   # Loschbour vs other populations

Odered_Loschbour <- Loschbour[order(Loschbour$F3),]  # $F3 means by f3 values

Loschbour_30 <- Odered_Loschbour[162:191,]

ggplot(Loschbour_30, aes(y = F3, x = factor(Loschbour_30$PopB, levels = Loschbour_30$PopB))) +
  geom_errorbar(aes(ymin = F3-3*StdErr, ymax = F3+3*StdErr), width = 0.0, size = 0.5, color = "grey") +
  geom_point(size = 2) +
  ylim(0.11, 0.225)+
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size =12, margin = margin (t = 9) ),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(0.2, "cm")) +
  geom_text(aes(y = F3-3*StdErr,label=PopB), hjust =1.1)
```
![](images/Loschbour_f3_plot.png)

**Figure 1**: Outgroup-f3(Yoruba; Loschbour, Y).

```
library(ggplot2)

LaBranaf3data <- read.table("LaBrana.f3", col.names=c("PopA", "PopB", "Outgroup", "F3", "StdErr", "Z", "SNPs"))

attach(LaBranaf3data)

LaBrana <- LaBranaf3data[PopA == "LaBrana1", ]   # Loschbour vs other populations

Odered_LaBrana <- LaBrana[order(LaBrana$F3),]  # $F3 means by f3 values

LaBrana_30 <- Odered_LaBrana[162:191,]

ggplot(LaBrana_30, aes(y = F3, x = factor(LaBrana_30$PopB, levels = LaBrana_30$PopB))) +
  geom_errorbar(aes(ymin = F3-3*StdErr, ymax = F3+3*StdErr), width = 0.0, size = 0.5, color = "grey") +
  geom_point(size = 2) +
  ylim(0.11, 0.225)+
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size =12, margin = margin (t = 9) ),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(0.2, "cm")) +
  geom_text(aes(y = F3-3*StdErr,label=PopB), hjust =1.1)
```
![](images/LaBrana_f3_plot.png)

**Figure 2**: Outgroup-f3(Yoruba; LaBrana1, Y).

```
library(ggplot2)

HungaryGamba_HG_f3data <- read.table("HungaryGamba_HG.f3", col.names=c("PopA", "PopB", "Outgroup", "F3", "StdErr", "Z", "SNPs"))

attach(HungaryGamba_HG_f3data)

HungaryGamba_HG <- HungaryGamba_HG_f3data[PopA == "HungaryGamba_HG", ]   # Loschbour vs other populations

Odered_HungaryGamba_HG <- HungaryGamba_HG[order(HungaryGamba_HG$F3),]  # $F3 means by f3 values

HungaryGamba_HG_30 <- Odered_HungaryGamba_HG[162:191,]

ggplot(HungaryGamba_HG_30, aes(y = F3, x = factor(HungaryGamba_HG_30$PopB, levels = HungaryGamba_HG_30$PopB))) +
  geom_errorbar(aes(ymin = F3-3*StdErr, ymax = F3+3*StdErr), width = 0.0, size = 0.5, color = "grey") +
  geom_point(size = 2) +
  ylim(0.11, 0.225)+
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size =12, margin = margin (t = 9) ),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(0.2, "cm")) +
  geom_text(aes(y = F3-3*StdErr,label=PopB), hjust =1.1)
```
![](images/HungaryGamba_HG_f3_plot.png)

**Figure 3**: Outgroup-f3(Yoruba; HungaryGamba_HG, Y).


### Reference:

1. Haak, Wolfgang, et al. "Massive migration from the steppe was a source for Indo-European languages in Europe." Nature 522.7555 (2015): 207.

2. Lazaridis, Iosif, et al. "Ancient human genomes suggest three ancestral populations for present-day Europeans." Nature 513.7518 (2014): 409.

3. Gamba, Cristina, et al. "Genome flux and stasis in a five millennium transect of European prehistory." Nature communications 5 (2014): 5257.

4. Olalde, Inigo, et al. "Derived immune and ancestral pigmentation alleles in a 7,000-year-old Mesolithic European." Nature 507.7491 (2014): 225.

