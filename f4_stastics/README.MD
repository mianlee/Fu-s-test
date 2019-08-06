### Western European hunter-gatherers(WHG)


*f4*(Loschbour, HungaryGamba_HG; *Test*, Chimp)


Making the paparmeter file:
```
nano par.WHG

genotypename: Haak_full.geno 
snpname: Haak_full.snp
indivname: Haak_full.ind
popfilename: f4_pop_WHG
```
Making ```popfilename``` file:

```
 awk '{print "Loschbour" "\t" "HungaryGamba_HG" "\t" $1 "\t" "Chimp"}' pop.list.txt > f4_pop_WHG

```
[pop.list.txt](Fu-s-test/Outgroup_f3_Test/data/pop.list.txt)

We can then run the D-statistic tests like so in the terminal:

```
qpDstat -p par.WHG > WHG.f4.result
```
