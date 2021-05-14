[![codecov](https://codecov.io/gh/balath/ipog-testing-algorithm/branch/master/graph/badge.svg?token=81V8EXIA17)](https://codecov.io/gh/balath/ipog-testing-algorithm)
![Scala CI](https://github.com/balath/ipog-testing-algorithm/actions/workflows/main.yml/badge.svg)

## Ipog algorithm

Work of implementation of Ipog algorithm for the Computing Engineering subject "Software testing"

Ipog Algorithm is describe in this [paper](IPOG_A_general_strategy_for_T-way_software_testing.pdf)

>Abstract 
>Most existing work on t-way testing has focused on 2-way (or pairwise) testing, which aims to detect faults 
caused by interactions between any two parameters. However, faults can also be caused by interactions involving 
more than two parameters. In this paper, we generalize an existing strategy, called In-Parameter-Order
(IPO), from pairwise testing to t-way testing. A major challenge of our generalization effort
is dealing with the combinatorial growth in the number of combinations of parameter values. We describe a
t-way testing tool, called FireEye, and discuss design decisions that are made to enable an
efficient implementation of the generalized IPO strategy. 
>
>Ipog Strategy
>The framework of the IPOG strategy can be described as follows: For a system with t or more parameters, the
IPOG strategy builds a t-way test set for the first t parameters, extends the test set to build a t-way test
set for the first t + 1 parameters, and then continues to extend the test set until it builds a t-way test set for
all the parameters.