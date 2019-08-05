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

There is no need to plot all comparision, so I only plotted the first 30 after sorting from highest f3 value to the lowest.


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

### 2. R package ```admixr``` 

```
# Load the admixr package
library(admixr)
library(tidyverse)

# Read in genotyping dataset: Haak_2013.geno, Haak_2013.snps, Haak_2013.ind.
f3data <- eigenstrat("/Users/LeeMian/Desktop/Fu/f3_test/Haak_2013")

# Assign population B
pops <- c("AA","Abkhasian","Adygei","Albanian","Alberstedt_LN",
          "Aleut","Algerian","Altaian","Ami","Armenian","Ashkenazi_Jew",
          "Atayal","Australian","Baalberge_MN","Balkar","Balochi","BantuKenya",
          "BantuSA","Basque","BedouinA","BedouinB","Belarusian","Bell_Beaker_LN",
          "Bengali","BenzigerodeHeimburg_LN","Bergamo","Biaka","Bolivian",
          "Bougainville","Brahui","Bulgarian","Burusho","Cambodian",
          "Canary_Islanders","Chechen","Chukchi","Chuvash",
          "Cochin_Jew","Corded_Ware_LN","Croatian", "Cypriot","Czech","Dai",
          "Datog","Daur","Dolgan","Druze","Egyptian","English","Esan","Eskimo",
          "Esperstedt_MN","Estonian","Ethiopian_Jew","Even","Finnish","French",
          "French_South","Gambian","Georgian","Georgian_Jew","Greek","GujaratiA",
          "GujaratiB","GujaratiC","GujaratiD","Hadza","Halberstadt_LBA","Han",
          "Han_NChina","Hazara","Hezhen","Hungarian","HungaryGamba_BA",
          "HungaryGamba_CA","HungaryGamba_EN","HungaryGamba_HG","HungaryGamba_IA",
          "Icelandic","Iceman","Iranian","Iranian_Jew","Iraqi_Jew","Italian_South",
          "Itelmen","Japanese","Jordanian","Ju_hoan_North","Kalash","Karelia_HG",
          "Karitiana","Karsdorf_LN","Khomani","Kikuyu","Kinh","Korean","Koryak",
          "Kostenki14","Kumyk","Kusunda","Kyrgyz","LBKT_EN","LBK_EN","LaBrana1",
          "Lahu","Lebanese","Lezgin","Libyan_Jew","Lithuanian","Loschbour","Luhya",
          "Luo","MA1","Makrani","Maltese","Mandenka","Mansi","Masai","Mayan","Mbuti",
          "Mende","Miao","Mixe","Mixtec","Mongola","Mordovian","Moroccan_Jew","Motala_HG",
          "Mozabite","Naxi","Nganasan","Nogai","North_Ossetian","Norwegian","Orcadian","Oroqen",
          "Palestinian","Papuan","Pathan","Piapoco","Pima","Punjabi","Quechua","Russian",
          "Saami_WGA","Saharawi","Samara_HG","Sardinian","Saudi","Scottish","Selkup","She",
          "Sicilian","Sindhi","Somali","Spain_EN","Spain_MN","Spanish","Spanish_North",
          "Starcevo_EN","Stuttgart","Surui","SwedenSkoglund_MHG","SwedenSkoglund_MN",
          "SwedenSkoglund_NHG","Syrian","Tajik_Pomiri","Thai","Tlingit","Tu","Tubalar",
          "Tujia","Tunisian","Tunisian_Jew","Turkish","Turkish_Jew","Turkmen","Tuscan",
          "Ukrainian","Ulchi","Unetice_EBA","Ust_Ishim","Uygur","Uzbek","Xibo","Yakut",
          "Yamnaya","Yemen","Yemenite_Jew","Yi","Yoruba","Yukagir","Zapotec")
          
# Run the outgroup-f3 test.
Loschbour.results <- f3(A = "Loschbour", B = pops, C = "Yoruba", data = f3data)
LaBrana.results <- f3(A = "LaBrana1", B = pops, C = "Yoruba", data = f3data)
HungaryGamba_HG.results <- f3(A = "HungaryGamba_HG", B = pops, C = "Yoruba", data = f3data)

# PLot Loschbour.results in R

attach(Loschbour.results)
Odered_Loschbour <- Loschbour.results[order(Loschbour.results$f3),]
Loschbour_30 <- Odered_Loschbour[164:193,]

ggplot(Loschbour_30, aes(y = f3, x = factor(Loschbour_30$B, levels = Loschbour_30$B))) +
  geom_errorbar(aes(ymin = f3-3*stderr, ymax = f3+3*stderr), width = 0.0, size = 0.5, color = "grey") +
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
  geom_text(aes(y = f3-3*stderr,label=B), hjust =1.1)

```
![](images/admixR_Loschbour_plot.png)

### Reference:

1. Haak, Wolfgang, et al. "Massive migration from the steppe was a source for Indo-European languages in Europe." Nature 522.7555 (2015): 207.

2. Lazaridis, Iosif, et al. "Ancient human genomes suggest three ancestral populations for present-day Europeans." Nature 513.7518 (2014): 409.

3. Gamba, Cristina, et al. "Genome flux and stasis in a five millennium transect of European prehistory." Nature communications 5 (2014): 5257.

4. Olalde, Inigo, et al. "Derived immune and ancestral pigmentation alleles in a 7,000-year-old Mesolithic European." Nature 507.7491 (2014): 225.

