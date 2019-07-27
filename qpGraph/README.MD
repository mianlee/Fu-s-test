## qpGraph: estimating admixture groups

Here, I am trying to repeat analyses, "Phylogeneticrelationships of ancient Eurasians" in Supplementary Information 8 in [Haak *et al*. (2015)](https://www.nature.com/articles/nature14317) using [```qpGraph```](https://github.com/DReichLab/AdmixTools/blob/master/README.QPGRAPH) in [AdmixTools](https://github.com/DReichLab/AdmixTools). In this section, the authors modeled the phylogenetic relationships of Eurasian hunter-gathers.

###  qpGraph overview:

The ```qpGraph``` program is part of the [AdmixTools](https://github.com/DReichLab/AdmixTools) pacakge. The ```qpGraph``` relies on an user-defined topology of the admixture graph and calculates the best-fitting admixture proportions and branch lengths based on the observed *f*-statistics. [Patterson *et al*. (2012)](https://www.genetics.org/content/192/3/1065.short), [CompPopGenWorkshop2019](https://github.com/stschiff/compPopGenWorkshop2019_docs/blob/master/contents/07_qpgraph/qpGraph.rst).

From [CompPopGenWorkshop2019](https://github.com/stschiff/compPopGenWorkshop2019_docs/blob/master/contents/07_qpgraph/qpGraph.rst), I quoted here:

The general approach of an admixture graph is to reconstruct the genetic relationships between different groups through a phylogenetic tree allowing for the addition of admixture events. The method operates on a defined graph’s topology and estimates f2, f3, and f4-statistic values for all pairs, triples, and quadruples of groups, compared to the expected allele frequency correlation of the tested groups. For a given topology, ```qpGraph``` provides branch lengths (in units of genetic drift) and mixture proportions. Groups that share a more recent common ancestor will covary more than others in their allele frequencies due to common genetic drift. We should keep in mind that the model is based on an unrooted tree and that while we show graphs with a selected outgroup as the root, the results should not depend on the root position.


The reconstructed graph is never meant to reflect a comprehensive population history of the region under study but the best fitted model to the limit of the available groups and method’s resolution.

As reported in the Method section of [Lipson and Reich 2017](https://academic.oup.com/mbe/article/34/4/889/2838774):

“Our usual strategy for building a model in ADMIXTUREGRAPH is to start with a small, well-understood subgraph and then add populations (either unadmixed or admixed) one at a time in their best-fitting positions. This involves trying different branch points for the new population and comparing the results. If a population is unadmixed, then if it is placed in the wrong position, the fit of the model will be poorer, and the inferred split point will move as far as it can in the correct direction, constrained only by the specified topology. Thus, searching over possible branching orders allows us to find a (locally) optimal topology. If no placement provides a good fit (in the sense that the residual errors are large), then we infer the presence of an admixture event, in which case we test for the best-fitting split points of the two ancestry components. After a new population is added, the topology relating the existing populations can change, so we examine the full model fit and any inferred zero-length internal branches for possible local optimizations.”








```graphviz``` installation:

```
conda install -c anaconda graphviz python-graphviz
```


```
dot -Tpng basic_model.han.dot -o basic_model.han.png
```


```
qpGraph -p par.basicmodel.han -g BasicModel.han -o basic_model.han.ggg -d basic_model.han.dot > basic_model.han.out
```




http://eurogenes.blogspot.com/2017/06/qpgraph-open-thread.html



https://github.com/stschiff/compPopGenWorkshop2019_docs/blob/master/contents/07_qpgraph/qpGraph.rst



**Reference**:
1. Patterson, Nick, et al. "Ancient admixture in human history." Genetics 192.3 (2012): 1065-1093.
