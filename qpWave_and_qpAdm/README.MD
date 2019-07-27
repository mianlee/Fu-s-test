#  *qpWave* and *qpAdm*: Admixture modelling

## Objectives

I am trying to repeat " Number of streams of migration into ancient Europe" in Supplementary Information 10, [Haak *et al*. (2015)](https://www.nature.com/articles/nature14317) using [*```qpWave```*](https://github.com/DReichLab/AdmixTools/blob/master/README.QpWave) in [AdmixTools](https://github.com/DReichLab/AdmixTools).

## Backgroup

**f3** and **D/f4** statistics are very useful tools to test simple phylogenetic hypotheses, such as whether certain population is sharing more genetic drift with one population than with other populations (outgroup-**f3**), whether the relationship of four test populations fits into a tree. However, and individual **f3** and **D/f4** - statistics cannot provide specifics of this "non-treeness": such as **1)** what is the magnitude and direction of gene flow, **2)** how many gene flows are required to model the relationship between the populations used, and **3)** whether the populations used are sufficient to model their relationships (or an unsampled lineage is required).

Both **qpWave** and **qpAdm** can be though of as more complex and powerful extensions of the basic ideas behind a simple **f4** statistic. Building upon the **f4** theory and generalizing it, **qpWave** makes it possible to find the lowest number of “streams of ancestry” between two groups of populations that is consistent with the data. Extending the concept of **f4** statistics even further, **qpAdm** allows to find the proportions of ancestry from a set of ancestral populations that contributed ancestry to our population of interest.

**QpWave** **and its derivative** **qpAdm** **are tools for summarizing information from multiple F-statistics to make such sophisticated inferences possible to a substantial degree. In this section, we focus on the following specific questions:**

1. Detecting the minimum number of independent gene pools to explain a set of target populations (qpWave).

2. Testing sufficienty of an admixture model within the resolution of data (qpAdm).

3. Estimating admixture proportions (qpAdm).

## Useful resources

I have learned the math behind **qpWave/qpAdm**, how to run **qpWave/qpAdm** and how to interpret results by reading:

1. "Reconstructing Native American population history", [Supplementary Information Note S6](data/Reich_et_al_2012_Supplementary.pdf) by [Reich *et al* 2012](https://www.nature.com/articles/nature11258). This paper first introduced the theoretical background of what later became qpWave.

2. "Massive migration from the steppe was a source for Indo-European languages in Europe", [Supplementary Information 10](https://media.nature.com/original/nature-assets/nature/journal/v522/n7555/extref/nature14317-s1.pdf) by [Haak *et al*. (2015)](https://www.nature.com/articles/nature14317), which gives a more consise overview of the **qpWave** method than Note S6 of [Reich *et al* 2012](https://www.nature.com/articles/nature11258), and also introduces the **qpAdm** methodology for estimating admixture proportions.

3. [CompPopGenWorkshop2019](https://github.com/stschiff/compPopGenWorkshop2019_docs/blob/master/contents/05_qpwave_qpadm/qpwave_qpadm.rst), which explain the math behind it and how to run **qpWave/qpAdm** with examples.

4. [Admixr - Tutorial](https://bodkan.net/admixr/articles/tutorial.html#qpwave-and-qpadm), which show you how to run **qpWave/qpAdm** in R.

5. Of course, if you want to use them, it’s crucial that you read the official documentation decribing the basic ideas of both methods, [available here](https://github.com/DReichLab/AdmixTools/blob/master/pdoc.pdf) and how to run them, [available here](https://github.com/DReichLab/AdmixTools/blob/master/README.QpWave).

## Install ADMIXTOOLS package 

You can download and install the Admixtools package by following the manual, [here](https://github.com/DReichLab/AdmixTools/blob/master/README).

or 

You can install the Admixtools package through ```conda``` :

```
conda install admixtools
```

## Application using genotype data from Haak *et al*. (2015)


### Basic idea:

Let ***T***, which stands for tested population, be a target population, and ***S***= {S1, S2,.....,Sn} a set of source populations. In the easiest case to consider, in which ***T*** is an admixture of populations of ***S***.

Thus, if ***T*** is admixted, as above, pick a set of outgroup populations ***O***, and 

1. Check, setting left populations L = ***S***, and  right populations ***O*** that the matrix ***X*** has full rank n-1.

2. Check, again setting L ={***T***, ***S***} that there is no strong evidence that the rank of ***T***.


In the following analyses, we will follow the idea described above of first testing a pair of source populations (left list ***U***) to ensure that they have rank=1 with respect to the outgroups, which shows that they are descended from two indenpendent streams of ancestry. We will then add a third population ***Test***, making U = {***U***, ***Test***}, showing that it does not increase the rank, and can thus be modelled as a mixture of the initial pair.

### 15 World Outgroups:

**Ami**: the Amis are an Austronesian ethnic group native to Taiwan.

**Biaka**: a nomadic Mbenga pygmy people of central Africa.

**Bougainville**: Bougainville Island is the main island of the Autonomous Region of Bougainville of Papua New Guinea.

**Chukchi**: the Chukchi, or Chukchee, are an indigenous people inhabiting the Chukchi Peninsula and the shores of the Chukchi Sea and the Bering Sea region of the Arctic Ocean within the Russian Federation. They speak the Chukchi language.

**Eskimo**: Eskimo or Eskimos are the indigenous peoples who have traditionally inhabited the northern circumpolar region from eastern Siberia (Russia) to Alaska (of the United States), Canada, and Greenland

**Han**: the Han Chinese are an East Asian ethnic group and nation native to China.

**Ju_hoan_North**: Juʼhoan, also known as Southern or Southeastern !Kung (Southeastern Ju), is the southern variety of the !Kung dialect continuum, spoken in northeastern Namibia and the Northwest District of Botswana. 

**Karitiana**: the Karitiana are an indigenous people of Brazil, whose reservation is located in the western Amazon.

**Kharia**: the Kharia are an Austroasiatic ethnic group from central India. This tribal ethnic group in India. They originally spoke the Kharia language, which belong to Austroasiatic languages. **No this population in the public dataset.**

**Mbuti**: Mbuti or Bambuti are one of several indigenous pygmy groups in the Congo region of Africa.

**Onge**: the Onge are one of the Andamanese indigenous peoples of the Andaman Islands. **No this population in the public dataset**

**Papuan**: the indigenous peoples of New Guinea, commonly called Papuans, are Melanesians. 

**She**: the She people are a Chinese ethnic group.

**Ulchi**: the Ulch are an indigenous people of the Russian Far East who speak a Tungusic language,

**Yoruba**: one of the three largest ethnic groups of Nigeria, concentrated in the southwestern part of that country

**P.s.** Punjabi: the Punjabis (Punjabi: پنجابی, ਪੰਜਾਬੀ) or Punjabi people are an Indo-Aryan ethnolinguistic group associated with the Punjab region in South Asia, specifically in the northern part of the Indian subcontinent, presently divided between Punjab, India and Punjab, Pakistan. **Here, I use Punjabi to subsitiute Indian population.** 


### "left" and "right" populations

Both "left" and "right" population lists are a simple text file, including one population name per line. In the EIGENSTRAT format input genotype data, population name matches entries in the third column of the ```.ind``` file. "Right" population list includes outgroup populations that are distantly but (potentially) differentially related to "left" populations. "Left" population list includes populations of your interest. For ```qpWave```, the order of left populations does not have specific meaning.


### Tesing the following hypotheses

#### 1. Westn and Eastern European hunter-gathers not a clade with respect to world outgoups


Generating one "left" and one "right" population list files using the following code:

```
refs="Loschbour Karelia_HG"
ogs="Ami Biaka Bougainville Chukchi Eskimo Han Ju_hoan_North Karitiana Mbuti Papuan Punjabi She Ulchi Yoruba"

echo ${refs} | sed s/" "/"\n"/g > left1.pops
echo ${ogs} | sed s/" "/"\n"/g > right.pops
```

We will use ```qpWave``` program in the ADMIXTOOLS package. Parameter file for ```qpWave``` has the structure, similar to parameter files for other programs included in the package. I named and saved the par file to ```par.1``` :

```
genotypename: Haak.geno
snpname: Haak.snp
indivname: Haak.ind
popleft: left1.pops
popright: right.pops
details: YES
maxrank: 7
```
Running the ```qpWave``` programs:

```
qpWave -p par.1 > qpWave.1.log
```

Results:

```
                                                      P
f4rank: 0  dof:  13  chisq:   236.987  tail:     3.20249971e-43
 
f4rank: 1  dof:   0  chisq:    0.000   tail:          1 

```

We first test the pair (Loschbour, Karelia_HG). Modle with rank = 0 is rejected (P = 3.20249971e-43 < 0.05), thus the WHG and EHG are descended from at least two ancestral populations.


**Note**:

I will do the same ```qpWave``` analysis above in **R** program. It is much easier in term of you don't have to make the popleft, popright and parameter file everytime.

Here, I am using the ```admixr``` package. This R package makes it possible to perform all stages of ADMIXTOOLS analyses entirely from R. See ```admixr``` maunal and tutorial, [here](https://bodkan.net/admixr/articles/tutorial.html). [Martin Petr *et al*. (2019)](https://academic.oup.com/bioinformatics/advance-article/doi/10.1093/bioinformatics/btz030/5298728)



**1. Installation**:

```
R version 3.6.1
Mac OS
xCode 10.3
```

Note that in order to use the ```admixr``` package, you need a working installation of ADMIXTOOLS! Download ADMIXTOOLS from [here](https://github.com/DReichLab/AdmixTools/). The install process is a little different from the manaul, because I had some problem to compile and install Admixtools and here is my solution for **Mac users**:

```
brew install gsl
brew install Brewsci/science/openblas

xcode-select --install
```

```
export LDFLAGS="-L/usr/local/opt/openblas/lib"
export CPPFLAGS="-I/usr/local/opt/openblas/include"
```

```
cd src
make install
```

```
echo "PATH=$PATH" > .Renviron
```

And I also installed Amixtools using ```conda```:

```
conda install -c bioconda admixtools 
```

To install admixr from GitHub you need to install the package devtools first. To do this, you can simply run (in R):

```
install.packages("devtools")
devtools::install_github("bodkan/admixr")
```

Furthermore, if you want to follow the examples in this vignette, you will need the tidyverse collection of packages for data manipulation manipulation and plotting, which you can install with:

```
install.packages("tidyverse")
```

When everything is ready, you can run the following code to load both packages in R:

```
library(admixr)
library(tidyverse)
```

**2. Runing qpWave**:

To run qpWave, you must provide a list of left and right populations:

```left = c("Loschbour", "Karelia_HG")```

```right = c("Ami", "Biaka", "Bougainville", "Chukchi", "Eskimo", "Han", "Ju_hoan_North","Karitiana", "Mbuti", "Papuan", "Punjabi", "She", "Ulchi", "Yoruba")```


Full R code:

```
# Load package 'admixr' and 'tidyverse'
library(admixr)
library(tidyverse)

# Locate your geno/ind/snp files
Haak <- eigenstrat("/Users/LeeMian/Desktop/Fu/qpWave/Haak")

# Run qpWave
result <- qpWave(
  left = c("Loschbour", "Karelia_HG"),
  right = c("Ami", "Biaka", "Bougainville", "Chukchi", "Eskimo", "Han", 
            "Ju_hoan_North","Karitiana", "Mbuti", "Papuan", "Punjabi", "She", "Ulchi", "Yoruba"),
  data = Haak
)

# View the result
result


# A tibble: 2 x 7
    rank    df    chisq     tail      dfdiff  chisqdiff  taildiff
    
     0      13     237.   3.20e-43      0        0       1.00e+ 0
     1      0       0     1.00e+ 0      13      237.     3.20e-43
```

How to read your genotyping dataset, for example I have ```Haak.geno```, ```Haak.ind```,```Haak.snp```:

```eigenstrat``` function use to specify the path and prefix of a trio of EIGENSTRAT snp/ind/geno files, format ```eigenstrat("/Path/To/Files/prefix)"```.

**As you can see, by comparing the results (using terminal and using R), they are exaxtly the same.**





**Reference**:

1. [Admixr - Tutorial](https://bodkan.net/admixr/articles/tutorial.html#qpwave-and-qpadm).

2. Petr, Martin, Benjamin Vernot, and Janet Kelso. "admixr–R package for reproducible analyses using ADMIXTOOLS (advance online)." Bioinformatics (2019).

3. [CompPopGenWorkshop2019](https://github.com/stschiff/compPopGenWorkshop2019_docs/blob/master/contents/05_qpwave_qpadm/qpwave_qpadm.rst).

4. Haak, Wolfgang, et al. "Massive migration from the steppe was a source for Indo-European languages in Europe." Nature 522.7555 (2015): 207.

5. Reich, David, et al. "Reconstructing native American population history." Nature 488.7411 (2012): 370.


