### Y haplogroup prediction from NGS data

Yleaf is the software for human Y-chromosomal haplogroup inference from next generation sequencing data that I used.

The Yleaf program can be downloaded from [Erasmus University Medical Center (Erasmus MC)](https://www6.erasmusmc.nl/genetic_identification/resources/). 

Yleaf v2 with an installation guide is publically available at: http://cluster15.erasmusmc.nl/fmb/Yleaf_v2/index.html .



#### Requirements
- Operating system: Linux only. Tested on Ubuntu 16.04LTS.
- Python, wget, git.
- Internet connection during installation (for downloading and extracting reference genomes).
- Data storage: For installation we recommend a storage capacity of > 10 GB.
#### Installation
1. Install dependencies (you can skip this step if these packages are already installed on your system)
 
 sudo apt-get install git-core
 sudo apt-get install python3.6
 sudo apt-get install mawk
 sudo apt-get install p7zip-full
 sudo apt-get install bwa

Samtools and bcftools
We recommend to install the newest version of SAMtools and BCFtools (e.g. >= 1.4.1)
1. download samtools and bcftools from : http://www.htslib.org/download/ .
2. ```tar -xjvf samtools.tar.bz2```
3. ```cd samtools-1.4.1/```
4. ```./configure```
5. ```make```
6. ```sudo make install```

2. Link to executables and download and build index from the reference genomes

``` python install.py```

3. To make the configurations effective immediately

 ```source ~/.profile```
