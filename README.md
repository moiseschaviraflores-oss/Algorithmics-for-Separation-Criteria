# Algorithmics for Separation Criteria 
Conditional independence can be decided by algorithms based on modular variations of graph reachability applied to directed state-graphs built via instructions provided in rule tables. The CIfly framework (see this [paper](https://doi.org/10.48550/arXiv.2506.15758) by Wienöbst et al., 2025) has been proposed for developing reachability-based algorithms to solve a variety of causal inference tasks. We rely on CIfly, available for R as <tt>ciflyr</tt>, and for Python as <tt>ciflypy</tt>, for implementing algorithms to detect graphical separation patterns in different types of probabilistic graphical models. 

## Installation
CIfly can be installed via CRAN in R
```r
# R installation via CRAN
install.packages("ciflyr")
```
And via pip in Python
```python
# Python installation with pip
pip install ciflypy
```
For Python the installation should not require any further dependencies. For R, the [Rust](https://rustup.rs/) toolchain must be installed if the package is build on your system, which is the case of Linux distributions.

## Example with d-separation 
We show how to check d-separation within R and Python. The algorithm corresponding to the following rule table (saved as <tt>d_connection_rule_table.txt</tt>) returns the set of all nodes $d$-connected to set $A$ by a set $C$. The rule table can be embedded into the code as a multi-line string or loaded from the file.
```r
EDGES --> <--
SETS A, C
START <-- AT A
OUTPUT ...

-->  | <--  | current in C
-->  | -->  | current not in C
<--  | -->  | current not in C
<--  | <--  | current not in C
```
Consider the directed acyclic graph (DAG) $G=(V,E)$ with $V=\\{1,2,3,4,6,7\\}$ and directed edges $E=\\{1\rightarrow2, 2\rightarrow3, 4\rightarrow2, 4\rightarrow5, 5\rightarrow6, 7\rightarrow5\\}$. Take $A=\\{1\\}$, and $C=\\{3, 6\\}$. 

In R we implement this task as follows.
```r
library(ciflyr)

# Path to the rule table .txt file
d_Connected_path <- "./d_connection_rule_table.txt"

# DAG stored in a list as an edge list matrix labeled by the edge type "-->"
G <- list("-->" = rbind(c(1, 2), c(2, 3), c(4, 2), c(4, 5), c(5, 6), c(7, 5)))

# We are interested in finding the set B of all nodes that are d-connected to A={1} by the set C={3,6}
Sets <- list("A" = c(1), "C" = c(3, 6))

# reach() is used to detect all nodes d-connected to the node 1 by the set {3,6}
reach(G, Sets, d_Connected_path)
```

And we can also do it in Python. 
```python
import ciflypy as cf

# Path to the rule table .txt file
d_Connected_path = "./d_connection_rule_table.txt"

# DAG stored in a dictionary as an edge list labeled by the edge type "-->"
G = {"-->": [(1, 2), (2, 3), (4, 2), (4, 5), (5, 6), (7, 5)]}

# We are interested in finding the set B of all nodes that are d-connected to A={1} by the set C={3,6}
sets = {"A": [1], "C": [3, 6]}

# cf.reach() is used to detect all nodes d-connected to the node 1 by the set {3,6}
cf.reach(G, sets, d_Connected_path)
```

Both code blocks yield the set $\\{1,2,3,4,5,6,7\\}$.

## Example with $*$-separation
The rules that determine whether a walk $w(a,b)$ in DAG $G$ is $\*$-connected by a set $C$ are quite similar to the ones that define a $d$-connected walk. However $\*$-connection only allows the presence of at most one collider (see this [paper](https://proceedings.mlr.press/v161/amendola21a.html) by Améndola et al. 2021). This time, we include the rule table as a multi-line string in R and Python (alternatively, the rule table file <tt>star_connection_rule_table.txt</tt> can be used as in above example) and show how to obtain the set of all nodes $*$-connected to set $A$ by the set $C$. 

In R we implement this task as follows.
```r
star_Connected <- "
EDGES --> <--
SETS A, C
COLORS before, after
START <-- [before] AT A
OUTPUT ... [after]

--> [before] | <-- [after]  | current in C
--> [before] | --> [before] | current not in C
<-- [before] | --> [before] | current not in C
<-- [before] | <-- [before] | current not in C
--> [after]  | --> [after]  | current not in C
<-- [after]  | --> [after]  | current not in C
<-- [after]  | <-- [after]  | current not in C
"

# reach() is used to detect all nodes *-connected to the node 1 by the set {3,6}
reach(G, Sets, star_Connected, tableAsString = TRUE)
```

And we can also do it in Python. 
```python
star_Connected = """
EDGES --> <--
SETS A, C
COLORS before, after
START <-- [before] AT A
OUTPUT ... [after]

--> [before] | <-- [after]  | current in C
--> [before] | --> [before] | current not in C
<-- [before] | --> [before] | current not in C
<-- [before] | <-- [before] | current not in C
--> [after]  | --> [after]  | current not in C
<-- [after]  | --> [after]  | current not in C
<-- [after]  | <-- [after]  | current not in C
"""

# cf.reach() is used to detect all nodes *-connected to the node 1 by the set {3,6}
cf.reach(G, sets, star_Connected, table_as_string = True)
```

Both code blocks yeild the set $\\{1,2,3,4,5,6\\}$. 
