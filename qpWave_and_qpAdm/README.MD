##  *qpWave* and *qpAdm*: Admixture modelling

### Objectives

I am trying to repeat " Number of streams of migration into ancient Europe" in Supplementary Information 10, [Haak *et al*. (2015)](https://www.nature.com/articles/nature14317) using [*```qpWave```*](https://github.com/DReichLab/AdmixTools/blob/master/README.QpWave) in [AdmixTools](https://github.com/DReichLab/AdmixTools).

### Backgroup

***f3*** and ***D/f4*** statistics are very useful tools to test simple phylogenetic hypotheses, such as whether certain population is sharing more genetic drift with one population than with other populations (outgroup-***f3***), whether the relationship of four test populations fits into a tree. However, and individual ***f3*** and ***D/f4*** - statistics cannot provide specifics of this "non-treeness": such as **1)** what is the magnitude and direction of gene flow, **2)** how many gene flows are required to model the relationship between the populations used, and **3)** whether the populations used are sufficient to model their relationships (or an unsampled lineage is required).

Both ***qpWave*** and ***qpAdm*** can be though of as more complex and powerful extensions of the basic ideas behind a simple ***f4*** statistic. Building upon the ***f4*** theory and generalizing it, ***qpWave*** makes it possible to find the lowest number of “streams of ancestry” between two groups of populations that is consistent with the data. Extending the concept of ***f4*** statistics even further, ***qpAdm*** allows to find the proportions of ancestry from a set of ancestral populations that contributed ancestry to our population of interest.

***QpWave*** **and its derivative** ***qpAdm*** **are tools for summarizing information from multiple F-statistics to make such sophisticated inferences possible to a substantial degree. In this section, we focus on the following specific questions:**

1. Detecting the minimum number of independent gene pools to explain a set of target populations (qpWave)

2. Testing sufficienty of an admixture model within the resolution of data (qpAdm)

3. Estimating admixture proportions (qpAdm)

### Useful resources

I have learned the math behind***qpWave*/*qpAdm*** , how to run ***qpWave*/*qpAdm*** and how to interpret results by reading:

1. "Reconstructing Native American population history", [Supplementary Information Note S6](data/Reich_et_al_2012_Supplementary.pdf) by [Reich *et al* 2012](https://www.nature.com/articles/nature11258). This paper first introduced the theoretical background of what later became qpWave.

2. "Massive migration from the steppe was a source for Indo-European languages in Europe", [Supplementary Information 10](https://media.nature.com/original/nature-assets/nature/journal/v522/n7555/extref/nature14317-s1.pdf) by [Haak *et al*. (2015)](https://www.nature.com/articles/nature14317), which gives a more consise overview of the ***qpWave*** method than Note S6 of [Reich *et al* 2012](https://www.nature.com/articles/nature11258), and also introduces the ***qpAdm*** methodology for estimating admixture proportions.

3. [CompPopGenWorkshop2019](https://github.com/stschiff/compPopGenWorkshop2019_docs/blob/master/contents/05_qpwave_qpadm/qpwave_qpadm.rst), which explain the math behind it and how to run ***qpWave*/*qpAdm*** with examples.

4. [Admixr - Tutorial](https://bodkan.net/admixr/articles/tutorial.html#qpwave-and-qpadm), which show you how to run ***qpWave*/*qpAdm*** in R.

5. Of course, if you want to use them, it’s crucial that you read the official documentation decribing the basic ideas of both methods, [available here](https://github.com/DReichLab/AdmixTools/blob/master/pdoc.pdf) and how to run them, [available here](https://github.com/DReichLab/AdmixTools/blob/master/README.QpWave).



### Application using genotype data from Haak *et al*. (2015)


**Basic idea**:

Let ***T***, which stands for tested population, be a target population, and ***S***= {S1, S2,.....,Sn} a set of source populations. In the easiest case to consider, in which ***T*** is an admixture of populations of ***S***.

Thus, if ***T*** is admixted, as above, pick a set of outgroup populations ***O***, and 

1. Check, setting left populations L = ***S***, and  right populations ***O*** that the matrix ***X*** has full rank n-1.

2. Check, again setting L ={***T***, ***S***} that there is no strong evidence that the rank of ***T***.


In the following analyses, we will follow the idea described above of first testing a pair of source populations (left list ***U***) to ensure that they have rank=1 with respect to the outgroups, which shows that they are descended from two indenpendent streams of ancestry. We will then add a third population ***Test***, making U = {***U***, ***Test***}, showing that it does not increase the rank, and can thus be modelled as a mixture of the initial pair.

**15 World Outgroups**:

**Ami**: the Amis are an Austronesian ethnic group native to Taiwan.

**Biaka**: a nomadic Mbenga pygmy people of central Africa.

**Bougainville**: Bougainville Island is the main island of the Autonomous Region of Bougainville of Papua New Guinea.

**Chukchi**: the Chukchi, or Chukchee, are an indigenous people inhabiting the Chukchi Peninsula and the shores of the Chukchi Sea and the Bering Sea region of the Arctic Ocean within the Russian Federation. They speak the Chukchi language.

**Eskimo**: Eskimo or Eskimos are the indigenous peoples who have traditionally inhabited the northern circumpolar region from eastern Siberia (Russia) to Alaska (of the United States), Canada, and Greenland

**Han**: the Han Chinese are an East Asian ethnic group and nation native to China.

**Ju_hoan_North**: Juʼhoan, also known as Southern or Southeastern !Kung (Southeastern Ju), is the southern variety of the !Kung dialect continuum, spoken in northeastern Namibia and the Northwest District of Botswana. 

**Karitiana**: the Karitiana are an indigenous people of Brazil, whose reservation is located in the western Amazon.

**Kharia**: the Kharia are an Austroasiatic ethnic group from central India. This tribal ethnic group in India. They originally spoke the Kharia language, which belong to Austroasiatic languages.

**Mbuti**: Mbuti or Bambuti are one of several indigenous pygmy groups in the Congo region of Africa.

**Onge**: the Onge are one of the Andamanese indigenous peoples of the Andaman Islands.

**Papuan**: the indigenous peoples of New Guinea, commonly called Papuans, are Melanesians. 

**She**: the She people are a Chinese ethnic group.

**Ulchi**: the Ulch are an indigenous people of the Russian Far East who speak a Tungusic language,

**Yoruba**: one of the three largest ethnic groups of Nigeria, concentrated in the southwestern part of that country












**Reference**:

1. [admixr - Tutorial](https://bodkan.net/admixr/articles/tutorial.html#qpwave-and-qpadm).

2. [compPopGenWorkshop2019](https://github.com/stschiff/compPopGenWorkshop2019_docs/blob/master/contents/05_qpwave_qpadm/qpwave_qpadm.rst).

3. Haak, Wolfgang, et al. "Massive migration from the steppe was a source for Indo-European languages in Europe." Nature 522.7555 (2015): 207.

4. Reich, David, et al. "Reconstructing native American population history." Nature 488.7411 (2012): 370.
