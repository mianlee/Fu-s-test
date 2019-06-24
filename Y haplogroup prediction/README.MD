## Y haplogroup prediction from NGS data

   **Yleaf** is the software for human Y-chromosomal haplogroup inference from next generation sequencing data that I used.

   The Yleaf program can be downloaded from [Erasmus University Medical Center (Erasmus MC)](https://www6.erasmusmc.nl/genetic_identification/resources/). 

   **Yleaf v2** with an installation guide is publically available at: http://cluster15.erasmusmc.nl/fmb/Yleaf_v2/index.html .
  
 ***Reference***: Ralf, A., Montiel González, D., Zhong, K., & Kayser, M. (2018). ***Yleaf: Software for Human Y-Chromosomal Haplogroup Inference from Next-Generation Sequencing Data***. Molecular biology and evolution, 35(5), 1291-1294.


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

### Run Yleaf program

Here are my system and program version:

```
Ubuntu 16.04.5 LTS-xenial
Python 3.6.7 
Samtools 1.9
Yleaf 2.0
```

When I fist run Yleaf, I had this error:

```
ImportError: No module named 'pandas'

#it due to the version of Python 2 and Python 3 under defaulty, even you installed Python 3. Try enther python in your terminal, see what version you got.
```
I fixed this by tring the following: 
```
## Python2 and Python3 uses different version of pandas.
## For Python3 you can install it using apt install:

sudo apt-get install python3-pandas

##In order to install it using pip, you should use pip3 for Python3:

sudo apt-get install python3-pip
sudo -H pip3 install pandas

## if you install minicoda, you also can try

conda install pandas
```
You check the [link1](https://askubuntu.com/questions/893587/install-pandas-for-python-3) and [link2](http://pandas.pydata.org/pandas-docs/stable/install.html) for detailed solutions.

