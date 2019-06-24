### Y haplogroup prediction from NGS data

Yleaf is the software for human Y-chromosomal haplogroup inference from next generation sequencing data that I used.

The Yleaf program can be downloaded from [Erasmus University Medical Center (Erasmus MC)](https://www6.erasmusmc.nl/genetic_identification/resources/). 
Yleaf v2 with an installation guide is publically available at: http://cluster15.erasmusmc.nl/fmb/Yleaf_v2/index.html .



#### Requirements
..* Operating system: Linux only. Tested on Ubuntu 16.04LTS.
 Python, wget, git.
 Internet connection during installation (for downloading and extracting reference genomes).
 Data storage: For installation we recommend a storage capacity of > 10 GB.
#### Installation
1. Install dependencies (you can skip this step if these packages are already installed on your system)
 apt-get install git-core
 apt-get install python3.6
apt-get install mawk
 apt-get install p7zip-full
 sudo apt-get install bwa
SAMtools
We recommend the newest version of SAMtools (e.g. >= 1.4.1)
1. wget https://github.com/samtools/samtools/releases/download/1.4.1/samtools-1.4.1.tar.bz2 -O \
samtools.tar.bz2
 2. tar -xjvf samtools.tar.bz2
 3. cd samtools-1.4.1/
 4. ./configure
 5. make
 6. make install
2. Link to executables and download and build index from the reference genomes

 python install.py
3. To make the configurations effective immediately
 source ~/.profile
