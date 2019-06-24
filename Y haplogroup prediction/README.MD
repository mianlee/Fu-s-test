## Y haplogroup prediction from NGS data

   **Yleaf** is the software for human Y-chromosomal haplogroup inference from next generation sequencing data that I used.

   The Yleaf program can be downloaded from [Erasmus University Medical Center (Erasmus MC)](https://www6.erasmusmc.nl/genetic_identification/resources/). 

   **Yleaf v2** with an installation guide is publically available at: http://cluster15.erasmusmc.nl/fmb/Yleaf_v2/index.html .
  
  **Reference**: Ralf, A., Montiel González, D., Zhong, K., & Kayser, M. (2018). **Yleaf: Software for Human Y-Chromosomal Haplogroup Inference from Next-Generation Sequencing Data**. Molecular biology and evolution, 35(5), 1291-1294.


#### Requirements
- Operating system: Linux only. Tested on Ubuntu 16.04LTS.
- Python, wget, git.
- Internet connection during installation (for downloading and extracting reference genomes).
- Data storage: For installation we recommend a storage capacity of > 10 GB.
#### Installation
1. Install dependencies (you can skip this step if these packages are already installed on your system)
 
   ```sudo apt-get install git-core```
 
   ```sudo apt-get install python3.6```
 
   ```sudo apt-get install mawk```
 
   ```sudo apt-get install p7zip-full```
 
   ```sudo apt-get install bwa```

   **Samtools and bcftools**
 
   We recommend to install the newest version of SAMtools and BCFtools (e.g. >= 1.4.1)
   1. download samtools and bcftools from : http://www.htslib.org/download/ .
   2. ```cd samtools-1.x```     # and similarly for bcftools
   3. ```tar -xjvf samtools.tar.bz2```
   4. ```cd samtools-1.4.1/```
   5. ```./configure```
   6. ```make```
   7. ```sudo make install```
 

2. Link to executables and download and build index from the reference genomes

   ``` python install.py```

3. To make the configurations effective immediately

   ```source ~/.profile```
   

### Use the 390k capture data from ["Massive migration from the steppe was a source for Indo-European languages in Europe"](https://www.nature.com/articles/nature14317)

1. Download the 390k capture data (.bam) for sample **I0061** as an example. [The link](https://www.ebi.ac.uk/ena/data/view/ERS665985), the first row Submitted file (FTP).
2. You will get the .bam file named ***I0061.390k.bam***.
3. Sort and index the .bam file.
```
# sorting
samtools sort I0061.390k.bam -o I0061.390k.sorted.bam #[-o out.bam]
# indexing
samtools index I0061.390k.sorted.bam
```
4. After sorting and indexing the .bam file, you will get two files named ***I0061.390k.sorted.bam*** and ***I0061.390k.sorted.bam.bai*** 

