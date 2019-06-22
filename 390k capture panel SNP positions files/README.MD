### Making a text file that contains all SNP positions in the 390k capture experiment.

All original files were downloaded from the paper **"Massive migration from the steppe was a source for Indo-European languages in Europe"**,  Online Table2. [Zipfiles](https://www.nature.com/articles/nature14317)


```
# Delete the first row and print the second and the thrid column, which include chromosome# and snp position.
sed '1d' OnlineTable2.San | awk '{print $2 "\t" $3}' > San
sed '1d' OnlineTable2.Yoruba | awk '{print $2 "\t" $3}' > Yoruba
sed '1d' OnlineTable2.compatibility | awk '{print $2 "\t" $3}' > compatibility
sed '1d' OnlineTable2.miscellaneous | awk '{print $2 "\t" $3}' > miscellaneous
# combine all four files
cat San Yoruba compatibility miscellaneous > 390k
# sort by first and second columns
sort -k1,1 -k2,2n 390k  > 390k.sorted

# the first 810 columns are "-", so I deleted all of them.
 grep -v "^-" 390k.sorted > 390k.bed



```
