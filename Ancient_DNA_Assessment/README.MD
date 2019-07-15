Since raw capture ancient DNA sequences/data are not available publically, some quality control checks on raw sequence data (using [FastQC](https://www.bioinformatics.babraham.ac.uk/projects/fastqc/)), demultiplexing of single or double indexed reads ([AdapterRemoval](https://github.com/MikkelSchubert/adapterremoval)), adapter/sequence trimming and sequence merging (using [AdapterRemoval](https://github.com/MikkelSchubert/adapterremoval), or [SeqPrep](https://github.com/jstjohn/SeqPrep)), map reads to reference genome and filter for quality (using [Bowtie2](http://bowtie-bio.sourceforge.net/bowtie2/index.shtml) or [BWA](http://bio-bwa.sourceforge.net/)) are not showing here. In the near future, I will use my raw data/find some raw data to show you all those analyses mentioned above.

Here, I will repeat analyses about how to assess the quility ancient DNA alignment starting with ```.BAM``` file and how to assess ancient DNA athenticity ( Damgae patterns / Contamination).


I will use ```.BAM``` files provided by Maria Nieves Colón in [her github](https://github.com/mnievesc/ENAH_curso_aDNA_2019/tree/master/Ex1_aDNA_BAM) to get familar with those softwares/programs, and then I will apply the same analyses to [Haak *et.al*. 2015](https://www.nature.com/articles/nature14317) dataset.




Visualizing and performing QC analysis on aDNA BAM files











### References:

1. Haak, Wolfgang, et al. "Massive migration from the steppe was a source for Indo-European languages in Europe." Nature 522.7555 (2015): 207.
