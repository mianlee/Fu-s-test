### Getting and checking full mtDNA sequences from ***sampleID.390k.bam*** file


The aligned sequences are available through the European Nucleotide Archive under accession number [PRJEB8448](https://www.ebi.ac.uk/ena/data/view/PRJEB8448),
in the form of ***.bam*** file.

For each ancient sample, it includes the aligned sequence for different libraries and a final combined aligned sequences, named ***sampleID.390k.bam***.

The ***sampleID.390k.bam*** file includes autosomal, Y and mitochondral DNA alignments. I am only extracting mtDNA alignments, the version of ```samtools``` and ```bcftools``` is **1.9**.


1. Download the ***390k.bam*** (FTP) file, for example ***I0061.390k.bam***.
2. Sorting and indexing the ***.bam*** file.

```
# sorting
samtools sort I0061.390k.bam -o I0061.390k.sorted.bam #[-o out.bam]
# indexing
samtools index I0061.390k.sorted.bam

# sorted and indexed file need to be co-existed to be able to extract mtDNA alignments

```
3. Extracting only mtDNA alignments.

```
samtools view -h I0061.390k.sorted.bam MT > I0061.390k.mt.sorted.bam

# -h, include the header in the output.
# output all alignments mapped to the reference sequence named `MT' (i.e. @SQ SN:MT).
# MT: reference for mitochondral DNA.


#check the file
samtools view -h I0061.390k.mt.sorted.bam

@HD	VN:1.3	SO:coordinate
@SQ	SN:1	LN:249250621
@SQ	SN:2	LN:243199373
@SQ	SN:3	LN:198022430
@SQ	SN:4	LN:191154276
@SQ	SN:5	LN:180915260
@SQ	SN:6	LN:171115067
.
.
.
@SQ	SN:X	LN:155270560
@SQ	SN:Y	LN:59373566
@SQ	SN:MT	LN:16569
.
.
NS500217:3:H0LGGAGXX:1:23302:19503:15206	16	MT	1	37	51M	*	0	0	GATCACAGGTCTATCACCCTATTAACCACTCACGGGAGCTCTCCATGCATT	FFFFFFF<FFFAFFFFFFFAFFAFFFFFFFFFFF.FFFFFFFFFFFFFFAF	XT:A:U	NM:i:0	X0:i:1	X1:i:0	XM:i:0	XO:i:0	XG:i:0	MD:Z:51	RG:Z:SR-Dosi123_merge__S0230__plus

# @SQ Reference sequence dictionary. The order of @SQ lines defines the alignment sorting order.

```
Because BAM files are compressed, we can't view them using our standard command line tools. Instead, we use ```samtools view```.


4. Indexing mitochondral DNA reference using bwa:

 download the .fasta file of the revised Cambridge Reference Sequence (rCRS) for mtDNA: [rCRS-fasta](https://www.ncbi.nlm.nih.gov/nuccore/251831106?report=fasta&log$=seqview).

 save the file name to mtref.fa and change the header **>NC_012920.1 Homo sapiens mitochondrion, complete genome** to **>MT**.

```
bwa index mtref.fa

```

5. Calling the consensus mitochondrial sequence.

```
bcftools mpileup -f mtref.fa I0061.390k.mt.sorted.bam | bcftools call -mv -Oz -o calls.vcf.gz
bcftools index calls.vcf.gz
cat mtref.fa | bcftools consensus calls.vcf.gz > I0061.mt.fa

```

The first mpileup part generates genotype likelihoods at each genomic position with coverage. **-f**, the faidx-indexed reference file in the FASTA format
The second call part makes the actual calls.
The -m switch tells the program to use the default calling method, the -v option asks to output only variant sites, finally the -O option selects the output format.
In this example we chosen compressed VCF (z), **-Oz**.



```
bcftools mpileup -f mtref.fa I0061.390k.mt.sorted.bam | bcftools filter -e 'MIN(DP)<2 & MAF[0]< 0.3'| bcftools call -mv -Oz -o calls.filter.vcf.gz
bcftools index calls.filter.vcf.gz
cat mtref.fa | bcftools consensus calls.filter.vcf.gz > I0061.mt.filter.fa
```

```bcftools filter -e 'MIN(DP)<2 & MAF[0]< 0.3'``` a minimum coverage of 2 and a minimum variant frequency of 0.7.

