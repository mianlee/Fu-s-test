## Principal Components Analysis (PCA)
Principal components analysis, PCA, is a one of the most useful statistical method used in population genetics to identify and visualize population structure in the distribution of genetic variation across geographical location and ethic backgroud, and genetic variation over time. In general, it allows breaking down high-dimensional datasets to two or more dimensions for visualisation in a two-dimensional space. 

### SmartPCA
The smartpca program uses a combination of Principal Component Analysis (PCA; a standard statistical method) and new statistics designed specifically for genomic data. The methods are described in detail in an excellent and accessible [article](https://journals.plos.org/plosgenetics/article?id=10.1371/journal.pgen.0020190), and [github manual](https://github.com/chrchang/eigensoft/blob/master/POPGEN/%23README%23)  under [Eigensoft package](https://github.com/chrchang/eigensoft) by the authors of the program.

### Genotype data
The dataset that I will work with is the [Fully public genotype dataset (354,212 SNPs)](https://reich.hms.harvard.edu/datasets) described in [Haak et al. Nature 2015](https://www.nature.com/articles/nature14317).

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

   The data itself comes in the so-called "EIGENSTRAT" format, which is defined in the [Eigensoft package](https://github.com/DReichLab/EIG) used by many tools. In this format, a genotype dataset consists of three files, usually with the following file endings:

   ***```*.snp```***
   
   The file containing the SNP positions. It consists of six columns: SNP-name, chromosome, genetic positions, physical position, reference allele, alternative allele.

   ***```*.ind```***
   
   The file containing the names of the individuals. It consists of three columns: Individual Name, Sex (encoded as M(ale), F(emale), or U(nknown)), and population name.

   ***```*.geno```***
   
   The file containing the genotype matrix, with individuals laid out from left to right, and SNP positions laid out from top to bottom.





### Operating System
I did all analyses above under Ubuntu 16.04.5 LTS-xenial, lunched through [VirtualBox](https://www.virtualbox.org/) on a Window 10 PC.

**Reference**: 

1. Stephan Schiffels, [compPopGenWorkshop2019](https://github.com/stschiff/compPopGenWorkshop2019_docs): [PCA](https://github.com/stschiff/compPopGenWorkshop2019_docs/tree/master/contents/03_pca).
2. [EVOLUTION AND GENOMICS](http://evomics.org/learning/population-and-speciation-genomics/2016-population-and-speciation-genomics/pca-exercise/).
3. Haak, W., Lazaridis, I., Patterson, N., Rohland, N., Mallick, S., Llamas, B., ... & Fu, Q. (2015). Massive migration from the steppe was a source for Indo-European languages in Europe. Nature, 522(7555), 207.
