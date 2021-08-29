[![codecov](https://codecov.io/gh/balath/ipog-testing-algorithm/branch/master/graph/badge.svg?token=81V8EXIA17)](https://codecov.io/gh/balath/ipog-testing-algorithm)
![Test CI](https://github.com/balath/ipog-testing-algorithm/actions/workflows/main.yml/badge.svg)

## Ipog algorithm

Implementation work of the Ipog algorithm for the Computer Engineering course "Software Testing".

The Ipog algorithm is described in this [paper](IPOG_A_general_strategy_for_T-way_software_testing.pdf).

>Abstract
>
>Most existing work on t-way testing has focused on 2-way (or pairwise) testing, which aims to detect faults 
caused by interactions between any two parameters. However, faults can also be caused by interactions involving 
more than two parameters. In this paper, we generalize an existing strategy, called In-Parameter-Order
(IPO), from pairwise testing to t-way testing. A major challenge of our generalization effort
is dealing with the combinatorial growth in the number of combinations of parameter values. We describe a
t-way testing tool, called FireEye, and discuss design decisions that are made to enable an
efficient implementation of the generalized IPO strategy. 
>
>Ipog Strategy
>
>The framework of the IPOG strategy can be described as follows: For a system with t or more parameters, the
IPOG strategy builds a t-way test set for the first t parameters, extends the test set to build a t-way test
set for the first t + 1 parameters, and then continues to extend the test set until it builds a t-way test set for
all the parameters.

## Running Implementations Tests
There are two ways to run ipog and ipog-D implementations tests:  
##### Github Actions
1. Go to [actions page](https://github.com/balath/ipog-testing-algorithm/actions) of the repository.
2. Select the last workflow (or which you want). 
3. In the upper-right corner of the workflow, use the **Re-run jobs** drop-down menu, and select **Re-run all jobs** for run tests again.
##### Scala Build Tool (sbt)
1. [Download](https://www.scala-sbt.org/download.html) and install sbt.
2. Fork and clone this repository.
3. Open your command line interface and go to project root folder and run:

`> sbt test`
