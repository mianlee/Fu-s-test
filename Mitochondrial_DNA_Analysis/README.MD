**Here I am trying to repeat all analyses related to mitochondrial DNA in [Haak *et al*. (2015)](https://www.nature.com/articles/nature14317) paper.**

1. Assessment of ancient DNA authenticity, search for evidence of contamination in the mitochondrial DNA.

2. Assignning mtDNA haplogroup from NGS data.

3. Population genetic analysis using mtDNA. 


**Goals**:

- Know how to estimate contamination rate using mitochondrial DNA

- Understand the basic workflow for determining human mtDNA lineages from NGS data.

- Be familiar with available online resources for researching human mtDNA haplogroup diversity and finding comparative datasets.

- Understand how ancient mtDNA results are interpreted and contextualized with other lines of evidence.



### mtDNA damage pattern

Here, I used NGS data from sample I0061, [here](https://www.ebi.ac.uk/ena/data/view/SAMEA3270053). I checked ancient DNA damage pattern for library **S0061** (non-UDG), **S0670** (half-UDG) and **S0230** (full-UDG) using [mapDamage2](https://ginolhac.github.io/mapDamage/). You can also download ```.bam``` file from here: [S0061](Data/S0061.bam), [S0670](Data/S0670.bam), [S0230](Data/S0230.bam).


If you use ```samtools view -h  *.bam``` to check thoe three files, you will find out **S0061.bam** contains both shallow shotgun sequences and mtDNA capture sequences, and **S0670** and **S0230** only contains capture mtDNA sequences.

I will extract all mtDNA capture sequences from ```S0061.bam``` file:

```
# sorting
samtools sort S0061.bam -o S0061.sorted.bam 

# indexing
samtools index S0061.sorted.bam

# sorted and indexed file need to be co-existed to be able to extract mtDNA alignments

samtools view -h S0061.sorted.bam MT > S0061.mt.bam

```

Indexing mitochondral DNA reference using bwa:

Download the ```.fasta``` file of the revised Cambridge Reference Sequence (rCRS) for mtDNA: rCRS-fasta(https://www.ncbi.nlm.nih.gov/nuccore/251831106?report=fasta&log$=seqview) and save as **rCRS.fa**.


```
bwa index rCRS.fa
```

I run the mapDamage with following command:

```
mapDamage -i S0061.mt.bam -r rCRS.fa
```

I receive the error: "FATAL ERROR: No sequences in common!". I think some information do not match between ```.bam ``` and newly indexed rCRS mtDNA reference. In oder to use [mapDamage2](https://ginolhac.github.io/mapDamage/), I have to convert ```.bam``` to ```.fastq``` and re-map those sequnce to rCRS mtDNA reference to generate new ```.sam``` file and finally convert ```.sam``` to ```.bam``` file.


```
# For library S0230.

bedtools bamtofastq -i S0230.bam -fq S0230.fq  # .bam to .fastq

bwa mem rCRS.fa S0230.fq > S0230.re.sam        # re-map fastq sequences to rCRS to get .sam file

samtools view -b S0230.re.sam > S0230.re.bam   # .sam to .bam file

mapDamage -i S0230.re.bam -r rCRS.fa              # run mapDamage 


# For library S0061, use different program to achieve the same goal.

samtools fastq S0061.mt.bam > S0061.mt.fq       # .bam to .fastq

bowtie2-build -f rCRS.fa rCRS.bowtie            # bulit bowtie rCRS index

bowtie2 -x rCRS.bowtie -U S0061.mt.fq -S S0061.mt.sam   # re-map fastq sequences to rCRS to get .sam file, -U for Unpaired

samtools view -b S0061.mt.sam > S0061.mt.re.bam  # .sam to .bam file

mapDamage -i S0061.mt.re.bam -r rCRS.fa          # run mapDamage



# For library S0670.

bedtools bamtofastq -i S0670.bam -fq S0670.fq  # .bam to .fastq

bwa mem rCRS.fa S0670.fq > S0670.re.sam        # re-map fastq sequences to rCRS to get .sam file

samtools fixmate -O bam S0670.re.sam S0670.re.bam   # .sam to .bam file

mapDamage -i S0670.re.bam -r rCRS.fa              # run mapDamage 

```

After I run all of these, I finally found out the mismatch is the header of the mtDNA refecence. In the ```.bam``` file, the sequences denoted as **MT**. So what you should do is just change the header of mtDNA reference to **>MT**, like: 

```
cat rCRS.fa

>MT
GATCACAGGTCTATCACCCTATTAACCACTCACGGGAGCTCTCCATGCATTTGGTATTTTCGTCTGGGGG
GTATGCACGCGATAGCATTGCGAGACGCTGGAGCCGGAGCACCCTATGTCGCAGTATCTGTCTTTGATTC
CTGCCTCATCCTATTATTTATCGCACCTACGTTCAATATTACAGGCGAACATACTTACTAAAGTGTGTTA
ATTAATTAATGCTTGTAGGACATAATAATAACAATTGAATGTCTGCACAGCCACTTTCCACACAGACATC
ATAACAAAAAATTTCCACCAAACCCCCCCTCCCCCGCTTCTGGCCACAGCACTTAAACACATCTCTGCCA
AACCCCAAAAACAAAGAACCCTAACACCAGCCTAACCAGATTTCAAATTTTATCTTTTGGCGGTATGCAC
TTTTAACAGTCACCCCCCAACTAACACATTATTTTCCCCTCCCACTCCCATACTACTAATCTCATCAATA
CAACCCCCGCCCATCCTACCCAGCACACACACACCGCTGCTAACCCCATACCCCGAACCAACCAAACCCC
AAAGACACCCCCCACAGTTTATGTAGCTTACCTCCTCAAAGCAATACACTGAAAATGTTTAGACGGGCTC

```
And then run:

```
bwa index rCRS.fa

mapDamage -i S0670.bam -r rCRS.fa
mapDamage -i S0230.bam -r rCRS.fa
mapDamage -i S0061.bam -r rCRS.fa
```

**Results**:

**S0061**, no UDG treatment library, [full plot](Data/S0061_DamagePattern.pdf).

<img src="Images/S0061_DamagePattern.PNG">

**S0670**, partial UDG treatment library, [full plot](Data/S0670_DamagePattern.pdf).

<img src="Images/S0670_DamagePattern.PNG">

**S0230**, full UDG treatment library, [full plot](Data/S0230_DamagePattern.pdf).

<img src="Images/S230_DamagePattern.PNG">


Those results match the ancient DNA dagamge pattern (no UDG treatment; partial UDG treatment; full UDG treatment), which mentioned in [Nadin Rohland et al. (2013)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4275898/), Partial uracil–DNA–glycosylase treatment for screening of ancient DNA.

**Note**: 

See my protocal How to install and use mapDamage, [Here](https://github.com/mianlee/Fu-s-test/tree/master/Ancient_DNA_Assessment).

See my notes on ancient DNA damage pattern, [Here](https://github.com/mianlee/Fu-s-test/tree/master/Ancient_DNA_Damage).


### Reference:

1. Haak, Wolfgang, et al. "Massive migration from the steppe was a source for Indo-European languages in Europe." Nature 522.7555 (2015): 207.
