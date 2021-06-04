# Data tests for SCBI mortality

## Table of tests 

level | category | applied to | test  | warning (W) or error (E) | coded | requires field fix? | auto fix (when applicable)
----  | ---- | ----  | ----  | ---- | ---- | ---- | ---- 
plot | census progress | all stems in census | percent trees censused | NA |  2021 | NA | NA 
plot | census progress | all stems in census | list or map of quadrats completed, with additional category for censused with fixes pending | NA |  not yet | NA | NA 
quadrat | completion check | newly censused quadrats | all trees censused |  E | 2021 | Y | NA 
quadrat  | consistency check | newly censused quadrats | no trees are duplicated |   W | 2021 | N | use latest record 
tree | completion check | newly censused trees (all) | `crown position` is recorded | E | 2021 | Y | NA 
tree | completion check | newly censused trees (all) |`percentage of crown intact` is recorded | E | 2021 | Y | NA 
tree | completion check | newly censused trees (all) |`percentage of crown living` is recorded | E | 2021 | Y | NA 
tree | consistency check | newly censused trees (all) | `percentage of crown living` ≤ `percentage of crown intact` | E | 2021 | initially | [issue 13](https://github.com/SCBI-ForestGEO/SCBImortality/issues/13)
tree | consistency check | newly censused trees (alive) | no FAD is selected; no record of wounded main stem, canker, or rotting trunk; DWR (dead with resprouts) not selected | E | 2021 | Y | NA
tree | consistency check | newly censused trees (AU) | DWR (dead with resprouts) not selected |E |  2021 | initially | ---
tree | completion check | newly censused trees (dead) | DBH measured | E | 2021 | Y | NA 
tree | consistency check - with previous | newly censused trees (dead) | `DBH` within 2cm of most recent census DBH | W | not yet | Y | NA
tree | completion check | newly censused trees (AU or dead) | at least one FAD is selected (OR level selected for `wounded main stem`,`canker,swelling,deformity`, `rotting main stem`)* | E |2021 | Y | NA 
tree | completion check | newly censused trees (AU or dead, with "wound" selected as FAD) | level selected for `wounded main stem` | E |2021 | Y | NA 
tree | completion check | newly censused trees (AU or dead, with "canker" selected as FAD) | level selected for `canker,swelling,deformity` |E | 2021 | Y | NA 
tree | completion check | newly censused trees (AU or dead, with "rotting stem" or "hollow stem" selected as FAD) | level selected for `rotting main stem` | E |2021 | Y | NA 
tree | completion check | newly censused trees (AU or dead) | at least one photo was taken | W | not yet | Y | NA 
tree | consistency check | newly censused trees (AU or dead, with level selected for `wounded main stem`)| "wound" selected as FAD, AU or dead selected as status | W| 2021 | N | add wound to FAD list*
tree | consistency check | newly censused trees (AU or dead, with level selected for `canker,swelling,deformity`)| "canker" selected as FAD | W| 2021 | N | add canker to FAD list* 
tree | consistency check | newly censused trees (AU or dead, with level selected for `rotting main stem`)| "rotting stem" or "hollow stem" selected as FAD| W| 2021 | N | add `rotting main stem` to FAD list* 
tree | consistency check | newly censused trees (any FAD selected, or level selected for `canker,swelling,deformity`, `wounded main stem` , or `rotting main stem`)| status selected as AU or dead | W| 2021 | N | change live to AU 
tree | consistency check - with previous | newly censused trees (A or AU) | tree was A or AU in previous year | W| not yet | Y | NA
tree | consistency check - with previous | newly censused trees (A or AU or DS) | tree was not DC in previous year | W| not yet | Y | NA


## not yet incorporated
- EAB census add-on
- thorough review

