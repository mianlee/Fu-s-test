## qpGraph: estimating admixture groups

The ```qpGraph``` program is part of the [AdmixTools](https://github.com/DReichLab/AdmixTools) pacakge. The ```qpGraph``` relies on an user-defined topology of the admixture graph and calculates the best-fitting admixture proportions and branch lengths based on the observed f-statistics. [Patterson *et al*. (2012)](https://www.genetics.org/content/192/3/1065.short), [CompPopGenWorkshop2019](https://github.com/stschiff/compPopGenWorkshop2019_docs/blob/master/contents/07_qpgraph/qpGraph.rst).





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
