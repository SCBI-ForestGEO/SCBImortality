# Data tests for SCBI mortality

## Table of tests 

level | category | applied to | test  | coded
----  | ---- | ----  | ----  | ---- 
plot | census progress | all stems in census | percent trees censused |  2021
quadrat | completion check | newly censused quadrats | all trees censused |  2021
quadrat  | consistency check | newly censused quadrats | no trees are duplicated |  2021
tree | completion check | newly censused trees (all) | `crown position` is recorded | 2021
tree | completion check | newly censused trees (all) |`percentage of crown intact` is recorded | not yet
tree | completion check | newly censused trees (all) |`percentage of crown living` is recorded | not yet
tree | consistency check | newly censused trees (all) | `percentage of crown living` ≤ `percentage of crown intact` | not yet
tree | consistency check | newly censused trees (alive) | no FAD is selected; no record of wounded main stem, canker, or rotting trunk; DWR (dead with resprouts) not selected | not yet
tree | consistency check | newly censused trees (AU) | DWR (dead with resprouts) not selected | not yet
tree | consistency check | newly censused trees (dead) | DBH measured | not yet
tree | completion check | newly censused trees (AU or dead) | at least one FAD is selected | not yet
tree | completion check | newly censused trees (AU or dead, with "wound" selected as FAD) | level selected for `wounded main stem` | not yet
tree | completion check | newly censused trees (AU or dead, with "canker" selected as FAD) | level selected for `canker,swelling,deformity` | not yet
tree | completion check | newly censused trees (AU or dead, with "rotting stem" or "hollow stem" selected as FAD) | level selected for `rotting main stem` | not yet
tree | completion check | newly censused trees (AU or dead) | at least one photo was taken | not yet
tree | consistency check | newly censused trees (AU or dead, with level selected for `wounded main stem`)| "wound" selected as FAD, AU or dead selected as status | not yet
tree | consistency check | newly censused trees (AU or dead, with level selected for `canker,swelling,deformity`)| "canker" selected as FAD | not yet
tree | consistency check | newly censused trees (AU or dead, with level selected for `rotting main stem`)| "rotting stem" or "hollow stem" selected as FAD| not yet
tree | consistency check | newly censused trees (any FAD selected, or level selected for `canker,swelling,deformity`, `wounded main stem` , or `rotting main stem`)| status selected as AU or dead | not yet


## not yet incorporated
- EAB census add-on
- thorough review
- consistency with past census (need to think about how strongly we constrain it here-- we probably want to generate warnings but not have tests fail. We do want to check DBH consistency)
