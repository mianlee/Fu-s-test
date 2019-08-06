### Western European hunter-gatherers(WHG)


***f4*(Loschbour, HungaryGamba_HG; *Test*, Chimp)**


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
[pop.list.txt](data/pop.list.txt)

We can then run the D-statistic tests like so in the terminal:

```
qpDstat -p par.WHG > WHG.f4.result
```


If I run all the 193 *f4* tests, the program will be killed. I did some google searces and found out " In some cases using popfilename the program uses an excessive amount of memory." So I sovlved this by divide the run into mulityple small runs and combine the result of each run. It is mentioned in the [```qpDstat``` manual](https://github.com/DReichLab/AdmixTools/blob/master/README.Dstatistics). 


```
qpDstat -p par.WHG -l 1 -h 50 > WHG.f4.result.1
qpDstat -p par.WHG -l 51 -h 101 > WHG.f4.result.2
qpDstat -p par.WHG -l 102 -h 152 > WHG.f4.result.3
qpDstat -p par.WHG -l 153 -h 193 > WHG.f4.result.4
```
Each run is really fast, like 1-2 mins.

Extraction final results:
```
cat WHG.f4.result.* | grep "result:" | awk '{print$2 "\t" $3 "\t" $4 "\t" $5"\t" $6 "\t" $7 }' > WHG.f4.result
```
[WHG.f4.result](data/WHG.f4.result)
