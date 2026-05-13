## Algorithmics for Separation Criteria 
The CIfly algorithmic framework (cite Wienoebst et al.) has been proposed to solve wide variety of tasks involving graphical objects. This= article focuses on how to to use the CIfly framework to detect other graphical separation criteria. The CIfly framework is available for R as <tt>ciflyr</tt>, and for Python as <tt>ciflypy</tt>.  

## Installation
CIfly can be installed via CRAN in R
```{r}
# R installation via CRAN
install.packages("ciflyr")
```
And via pip in Python
```{python}
# Python installation with pip
pip install ciflypy
```
For Python the installation should not require any further dependencies. For R, the [Rust](https://rustup.rs/) toolchain must be installed if the package is build on your system, which is the case of Linux distributions.

## Example with d-separation 
Now show how to detect d-separation with CIfly in R and Python. The CIfly algorithm specified by the following rule table (saved in the file d_connection_rule_table.txt) returns all nodes d-connected to set A by a set C.
```{r}
EDGES --> <--
SETS A, C
START <-- AT A
OUTPUT ...

-->  | <--  | current in C
-->  | -->  | current not in C
<--  | -->  | current not in C
<--  | <--  | current not in C
```
