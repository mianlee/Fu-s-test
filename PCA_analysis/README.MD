## Principal Components Analysis (PCA)
Principal components analysis, PCA, is a one of the most useful statistical method used in population genetics to identify and visualize population structure in the distribution of genetic variation across geographical location and ethic backgroud, and genetic variation over time. In general, it allows breaking down high-dimensional datasets to two or more dimensions for visualisation in a two-dimensional space. 

### SmartPCA
The smartpca program uses a combination of Principal Component Analysis (PCA; a standard statistical method) and new statistics designed specifically for genomic data. The methods are described in detail in an excellent and accessible [article](https://journals.plos.org/plosgenetics/article?id=10.1371/journal.pgen.0020190), and [github manual](https://github.com/chrchang/eigensoft/blob/master/POPGEN/%23README%23)  under [Eigensoft package](https://github.com/chrchang/eigensoft) by the authors of the program.

### Genotype data
The dataset that I will work with is the [Fully public genotype dataset (354,212 SNPs)](https://reich.hms.harvard.edu/datasets) described in [Haak et al. Nature 2015](https://www.nature.com/articles/nature14317). 

The dataset contains **2,071** individuals, each represented by **354,212 autosomal*** single nucleotide polymorphisms (SNPs). Those SNPs have exactly two different alleles, and each individual has one of four possible values at each genotype: homozygous reference, heterozygous, homozygous alternative, or missing. Those four values are encoded 2, 1, 0 and 9 respectively.

1. Downlaod the genotype data from [David Reich Lab Dataset](https://reich.hms.harvard.edu/datasets) webpage. 

2. You will get a zip file named : ***Haak2015PublicData.tar.gz***.
    ```
    # Move to the folder that contains the downloaded genotype data file, and type 'ls' commmand to display the file name.
    
    ls
    Haak2015PublicData.tar.gz
    ```
3. Unzip and extract a ***.tar.gz*** file in the terminal.

   ```
   tar -xvzf Haak2015PublicData.tar.gz
   
   # The followings will show on your screen
   data.geno
   data.ind
   data.pops
   data.snp
   README
   ```
   To explain a little further, ```tar``` collected all the files into one package, ***.tar*** file. The gzip program applied compression, hence the ***.gz*** extension. So the command does a couple things:

   - ```f```: this must be the last flag of the command, and the tar file must be immediately after. It tells tar the name and path of the compressed file.
   - ```z```: tells tar to decompress the archive using gzip
   - ```x```: tar can collect files or extract them. x does the latter.
   - ```v```: makes tar talk a lot. Verbose output shows you all the files being extracted.
4. You will end up having five files: ***data.geno***, ***data.ind***, ***data.snp***, ***data pops*** and ***README***.

   The data itself comes in the so-called "PACKEDANCESTRYMAP" format, which is defined in the [Eigensoft package](https://github.com/DReichLab/EIG) used by many tools. The "PACKEDANCESTRYMAP" format can be also converted to other formats using the ["convertf"](https://github.com/chrchang/eigensoft/tree/master/CONVERTF) utility of the EIGENSOFT software package: [see details](https://reich.hms.harvard.edu/software/InputFileFormats).
   
   
   In this format, a genotype dataset consists of three files, usually with the following file endings:

   ***```*.snp```***
   
   The file containing the SNP positions. It consists of six columns: SNP-name, chromosome, genetic positions, physical position, reference allele, alternative allele.

   ***```*.ind```***
   
   The file containing the names of the individuals. It consists of three columns: Individual Name, Sex (encoded as M(ale), F(emale), or U(nknown)), and population name.

   ***```*.geno```***
   
   The file containing the genotype matrix, with individuals laid out from left to right, and SNP positions laid out from top to bottom.

  
   ***```README```***
    ```
    cat README
   
    # This genotype dataset includes present-day humans published by Lazaridis et al. (2014), 
     69 ancient humans by Haak et al. (2015), 
     other ancient humans from the literature included in the analysis by Haak et al. (2015), 
     and other ancient individuals (e.g., archaics).

    ```
   
   ***```data.pops```***
  
   This file was made by the authors and containing sample size (1st column) and the name of each population group (2rd column).
   
   ```
   head data.pops 
   ```
      12 AA
      
      9 Abkhasian 
      
     17 Adygei
     
      1 AG2 
      
      6 Albanian 
      
      1 Alberstedt_LN 
      
      7 Aleut 
      
      1 Aleutian 
      
      7 Algerian 
      
      1 Altai
      
      
      
     ***Calculating the total number of indivduals by add the first column up***
     ```
     awk '{sum += $1} END {print sum}' data.pops
     
     2071  # There are 2071 individuals in this dataset in total
     ```
     
### How PCA analysis works

To understand how PCA works, consider a single individual and its representation by its 354,212 markers. Formally, each individual is a point in a 354,212-dimensional space, where each dimension can take only the three possible genotypes indicated above, or have missing data. To visualise this high-dimensional dataset, we would like to project it down to two dimensions. But as there are many ways to project the shadow of a three-dimensional object on a two dimensional plane, there are many (and even more) ways to project a 354,212-dimensional cloud of points to two dimensions. What PCA does is figuring out the "best" way to do this project in order to visualise the major components of variance in the data.

**Reference**: Stephan Schiffels, [compPopGenWorkshop2019](https://github.com/stschiff/compPopGenWorkshop2019_docs): [PCA](https://github.com/stschiff/compPopGenWorkshop2019_docs/tree/master/contents/03_pca). I quoted here with slightly change.



### Preparing the file for SmartPCA analysis

According to the paper [(Haak et al, 2015)](https://www.nature.com/articles/nature14317), - supplementary information 5 -, the PCAanalysis was conducted using ```smartpca``` on 777 present-day West Eurasians and 92 ancient individuals.


As I mentioned above, the genotype dataset itself from [(Haak et al, 2015)](https://www.nature.com/articles/nature14317) contains **2,071** individuals, which belong to **224** population groups/catergories. I need to figure out which 777 present-day West Eurasians and 92 ancient individuals were used by the authors for the PCA analysis.

Based on the Figure lengends of Figure 2a and Figure S5.1, I found out/identified those 777 West Eurasians ( from 59 population) and 92 ancient individuals (from 33 population), the details were summarized in the ***SampleInformation.xlsx*** under this folder.


**1. Figure 2a and Figure S5.1**

  The first PCA analysis was conducted on 777 presnet-day West Eurasians and 92 ancient individuals only at **66,225 transversion SNPs**. 


  **A word about DNA damage**

  If the samples you are analysing are ancient samples, the DNA will likely contain DNA damage, so C->T changes which are seen as C->T and G->A substitutions. There are two ways how to deal with that. First, if your data is not UDG-treated, so if it contains the full damage, you should restrict your analysis to Transversion SNPs only. If your data is UDG-treated, you will have much less damage, but you can still see damaged sites in particular in the boundary of the reads in your BAM-file. In that case, you probably want to make a modified bam file for each sample, where the first 2 bases on each end of the read are clipped. [Reference and See details](https://gaworkshop.readthedocs.io/en/latest/contents/04_genotyping/genotyping.html)

  ***Haak et al.*** claimed that I quoate here: "We excluded tranmsition SNPs in this analysis as non-UDG-treated ancient DNA libraries from the literature had high rates of damage causing apparent C->T and G->A substitutions,which potentially leads to misleading results, for example by causing ancient DNA samples to apper aas extreme outliers, dominating one the first two pricipal components."


  Finding transversion SNPs in ***data.snp***:

  ```
  cat data.snp | grep -v -E  "G A|A G|C T|T C" | wc -l

  66,225

  cat data.snp | grep -v -E  "G A|A G|C T|T C" | head
  ```

   Affx-4690339      1         0.029735         1205155  A  C
   
   Affx-5354053      1         0.032495         1519068  A  C
   
   Affx-6395872      1         0.040110         1942033  C  A
   
   Affx-6411981      1         0.040627         1948400  C  A
    
   Affx-7257140      1         0.044968         2164935  G  T
   
   Affx-50018429     1         0.046476         2223669  G  T
   .
   .
   .
   .
   .
   ```
   cat data.snp | grep -v -E  "G A|A G|C T|T C" > transversionSNP.txt
   
   # The 'transversionSNP.txt' will be used to extract all transversion SNPs using Plink later.
   ```
   
   **Now we can extract those 869 individuals (777 present-day West Eurasians and 92 ancient individuals), and 66,225 transvrsion SNPs from the full genotype dataset**. 
   
   In order to extract certain individuals and/or SNPs from the full dataset, you need to convert 'PACKEDANCESTRYMAP' format (or any other [EIGENSOFT](https://github.com/chrchang/eigensoft/tree/master/CONVERTF) format, such as ANCESTRYMAP, EIGENSTRAT, PED) to 'PACKEDPED' format cause the [PLINK](https://www.cog-genomics.org/plink2) program will do and does a better job. 
   
   Note: The EIGENSOFT [PED](https://github.com/chrchang/eigensoft/tree/master/CONVERTF) format is equivalent to PLINK [PED](http://zzz.bwh.harvard.edu/plink/data.shtml#ped) format. The EIGENSOFT [PACKEDPED](https://github.com/chrchang/eigensoft/tree/master/CONVERTF) format is equivalent to PLINK [BED](http://zzz.bwh.harvard.edu/plink/data.shtml#bed) format, as known as Binary PED files. To do individual and SNPs extraction, you need to provide Binary PED files (*.bed, *.bim, *.fam) to the PLINK.
   
   
   ***EIGENSOFT 6.1.4***
   
   ***PLINK 1.9***
   
   ```
   # Convert the PACKEDANCESTRYMAP format to PACKEDPED.
   # Use nano text editor in the terminal to make the par file named: " par.PACKEDANCESTRYMAP.PACKEDPED ".
   
   nano par.PACKEDANCESTRYMAP.PACKEDPED
   
   # And type the following in the text editor and save it.
   
   
   genotypename: data.geno
   snpname: data.snp
   indivname: data.ind
   outputformat: PACKEDPED
   genotypeoutname: WestEuroTransver.bed
   snpoutname: WestEuroTransver.bim
   indivoutname: WestEuroTransver.fam

   ```
   
   ```
   cat par.PACKEDANCESTRYMAP.PACKEDPED
    
   genotypename: data.geno
   snpname: data.snp
   indivname: data.ind
   outputformat: PACKEDPED
   genotypeoutname: data.bed
   snpoutname: data.bim
   indivoutname: data.fam
   
   ```
   
   I used EIGENSOFT 6.1.4, you can download it [here](https://data.broadinstitute.org/alkesgroup/EIGENSOFT/).  ```convertf``` is a program in the EIGENSOFT package, see [the manual](https://github.com/chrchang/eigensoft/tree/master/CONVERTF) in github.
   
   ```
   /Path/To/EIG-6.1.4/bin/convertf -p par.PACKEDANCESTRYMAP.PACKEDPED 
   
   
   packed geno read OK
   numvalidind:   2071  maxmiss: 2071001
   packedped output
   ##end of convertf run
   ```
   
  You will get three Binary PED files ***data.bed, data.bim, data.fam***. If you check the ***data.fam*** file, I noticed that the family/population group information (the first column, showing only numbers) were lost during the format conversion. I found the population group information in the ***data.ind** file, and updated the ***data.fam*** file in excel manually. I also uploaded the modified ***data.fam*** file in the folder.
  
  ```
  # The original data.fam file
  
  head data.fam
  
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

  # The updated data.fam file
  
  head data.fam
  
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
   
   
   
   
   
   
   
   
   ```
   4.14 Extract a subset of SNPs: file-list options
To extract only a subset of SNPs, it is possible to specify a list of required SNPs and make a new file, or
perform an analysis on this subset, by using the command
plink --file data --extract mysnps.txt
where the file is just a list of SNPs, one per line, e.g.
snp005
snp008
snp101
```

```
To keep only certain individuals in a file, use the option:
plink --file data --keep mylist.txt
where the file mylist.txt is, as for the --remove command, just a list of Family ID / Individual ID
pairs, one set per line, i.e. one person per line. (fields can occur after the 2nd column but they will be
ignored – i.e. you could use a FAM file as the parameter of the --keep command, or have comments in the
file. For example
F101 1
F1001 2 B
F3033 1 A Drop this individual because of consent issues
F4442 22
```

   
  ```
  plink --file WestEuroTransver --make-bed --out WestEuroTransver
  
  8703 MB RAM detected; reserving 4351 MB for main workspace.
.ped scan complete (for binary autoconversion).
Performing single-pass .bed write (354212 variants, 2071 people).
--file: WestEuroTransver-temporary.bed + WestEuroTransver-temporary.bim +
WestEuroTransver-temporary.fam written.
354212 variants loaded from .bim file.
2071 people (1307 males, 764 females) loaded from .fam.
2071 phenotype values loaded from .fam.
Using 1 thread (no multithreaded calculations invoked).
Before main variant filters, 2071 founders and 0 nonfounders present.
Calculating allele frequencies... done.
Total genotyping rate is 0.972586.
354212 variants and 2071 people pass filters and QC.
Among remaining phenotypes, 0 are cases and 2071 are controls.
--make-bed to WestEuroTransver.bed + WestEuroTransver.bim +
WestEuroTransver.fam ... done.

```
   
   
   
   
   
     
### Operating System and program version
I did all analyses above under Ubuntu 16.04.5 LTS-xenial, lunched through [VirtualBox](https://www.virtualbox.org/) on a Window 10 PC.
   
EIGENSOFT 6.1.4
   
PLINK 1.9

**Reference**: 

1. Stephan Schiffels, [compPopGenWorkshop2019](https://github.com/stschiff/compPopGenWorkshop2019_docs): [PCA](https://github.com/stschiff/compPopGenWorkshop2019_docs/tree/master/contents/03_pca).
2. [EVOLUTION AND GENOMICS](http://evomics.org/learning/population-and-speciation-genomics/2016-population-and-speciation-genomics/pca-exercise/).
3. Haak, W., Lazaridis, I., Patterson, N., Rohland, N., Mallick, S., Llamas, B., ... & Fu, Q. (2015). Massive migration from the steppe was a source for Indo-European languages in Europe. Nature, 522(7555), 207.
4. Lazaridis, I., Patterson, N., Mittnik, A., Renaud, G., Mallick, S., Kirsanow, K., ... & Berger, B. (2014). Ancient human genomes suggest three ancestral populations for present-day Europeans. Nature, 513(7518), 409.
5. Purcell, S., Neale, B., Todd-Brown, K., Thomas, L., Ferreira, M. A., Bender, D., ... & Sham, P. C. (2007). PLINK: a tool set for whole-genome association and population-based linkage analyses. The American journal of human genetics, 81(3), 559-575.
