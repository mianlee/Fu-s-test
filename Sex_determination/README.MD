The original reference paper for Sex determination in ancient human remains: **"Accurate sex identification of ancient human remains using DNA shotgun sequencing", [Link](https://www.sciencedirect.com/science/article/pii/S0305440313002495)**.


The Python script to compute Ry statistic that also performs sex identification was downloaded from the article mentioned above and named as ***Ry_compute.py***.

Usage:		python Ry_compute.py <SAM formatted data from stdin>

Example:	

```samtools view -q 30 mybamfile.bam | python Ry_compute.py```

Notes:

```samtools view -q```:  **-q** INT,  Skip alignments with MAPQ smaller than INT 0.

MAPQ: [MAPping Quality](http://www.acgt.me/blog/2014/12/16/understanding-mapq-scores-in-sam-files-does-37-42).


FOr sample I0061, I run the following command to the sorted sam file.
```
samtools view -q 30 I0061.390k.sorted.bam | python yjasc_3752_ry_compute.py

#getting the results
Nseqs	   NchrY+NchrX	   NchrY	  R_y	      SE	    95% CI	    Assignment
5676448    68583 	    13177 	 0.1921   0.0015    	0.1892-0.1951 	    XY

```


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
Change the header of both files, '**>chrX** and **>chrY'**, to '**>X** and **>Y**'.
```
gsed -i 's/chrX/X/g' chrX.fa
gsed -i 's/chrY/Y/g' chrY.fa
```

Extacting Y alignment sequences from 390k.bam file.
```
samtools view -h I0061.390k.sorted.bam Y > I0061.390k.Y.sorted.bam
```

bcftools mpileup -q 30 -Q 30 -f chrY.fa I0061.390k.Y.sorted.bam | bcftools call -mv -Oz -o Y.filter.calls.vcf.gz



-q, -min-MQ INT
Minimum mapping quality for an alignment to be used [0]

-Q, --min-BQ INT
Minimum base quality for a base to be considered [13

