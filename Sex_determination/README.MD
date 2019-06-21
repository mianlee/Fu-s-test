The sex of the 69 individuals was determined by examining the nunber of X chromosome and Y chromosome SNPs (the ratio) targeted by the 390k capture reagent. 

### Some notes:

GRCh37: Genome Reference Consortium Human Build 37.

The different between reference GRCh37 and hg19, check this [link](https://software.broadinstitute.org/gatk/documentation/article?id=23390).

Download **build ucsc.hg19.fasta** as reference can be done following this instruction: https://github.com/creggian/ucsc-hg19-fasta.

### Getting the reference human genome
The reference sequences for Chromosome X and Y were downloaded from : [UCSC hg19 asseembly](http://hgdownload.cse.ucsc.edu/goldenpath/hg19/chromosomes/).

1. The sequence format is ***.fasta***.
2. Human genome assembly version: **hg19 / GRCh37**.
3. UCSC: "University of California at Santa Cruz".

I am getting two zip files: **chrX.fa.gz** and **chrY.fa.gz**.

### Unzip the two zip files
```
gunzip chrX.fa.gz chrY.fa.gz

ls
chrX.fa	chrY.fa
```
Change the header of both files, '**>chrX** and **>chrY'**, to '**>X** and **>Y**'





