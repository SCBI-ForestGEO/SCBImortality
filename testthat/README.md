# Data tests for mortality census

## Table of tests 

No|level | category | applied to | test  | warning (W) or error (E) | coded | requires field fix? | auto fix (when applicable) | core or SCBI add-on?
----  |----  | ---- | ----  | ----  | ---- | ---- | ---- | ----  | ---- 
1|plot | census progress | all stems in census | percent trees censused | NA |  2021 | NA | NA | core
2|plot | census progress | all stems in census | list or map of quadrats completed, with additional category for censused with fixes pending | NA |  2021 | NA | NA | core 
3|quadrat | completion check | newly censused quadrats | all trees censused |  E | 2021 | Y | NA | core
4|quadrat  | consistency check | newly censused quadrats | no trees are duplicated |   W | 2021 | N | use latest record | core
5|tree | completion check | newly censused trees (A, AU, DS) | `crown position` is recorded | E | 2021 | Y | NA | core
6|tree | completion check | newly censused trees (A, AU, DS) |`percentage of crown intact` is recorded | E | 2021 | Y | NA | core
7|tree | completion check | newly censused trees (A, AU, DS) |`percentage of crown living` is recorded | E | 2021 | Y | NA | core
8|tree | consistency check | newly censused trees (A, AU, DS) | `percentage of crown living` ≤ `percentage of crown intact` | E | 2021 | initially | [issue 13](https://github.com/SCBI-ForestGEO/SCBImortality/issues/13)| core
9|tree | consistency check | newly censused trees (DS or DC) | `percentage of crown living` = 0 | E | 2021 | Y | NA| core
10|tree | consistency check | newly censused trees (alive) | no FAD is selected; no record of wounded main stem, canker, or rotting trunk; DWR (dead with resprouts) not selected | E | 2021 | sometimes | if `percentage of crown living`>0, change status to AU; otherwise requires field check (to determine DS vs DC) | core
11|tree | consistency check | newly censused trees (AU) | DWR (dead with resprouts) not selected |E |  2021 | initially | ---| core
12|tree | completion check | newly censused trees (dead) | DBH measured | E | 2021 | Y | NA | core
13|tree | consistency check - with previous | newly censused trees (dead) | `DBH` within 2cm of most recent census DBH | W | 2021 | Y | NA| SCBI
14|tree | completion check | newly censused trees (AU or dead) | at least one FAD is selected (OR level selected for `wounded main stem`,`canker,swelling,deformity`, `rotting main stem`)* | E |2021 | Y | NA | core
15|tree | completion check | newly censused trees (AU or dead, with "wound" selected as FAD) | level selected for `wounded main stem` | E |2021 | Y | NA | core
16|tree | completion check | newly censused trees (AU or dead, with "canker" selected as FAD) | level selected for `canker,swelling,deformity` |E | 2021 | Y | NA | core
17|tree | completion check | newly censused trees (AU or dead, with "rotting stem" or "hollow stem" selected as FAD) | level selected for `rotting main stem` | E |2021 | Y | NA | core
18|tree | completion check | newly censused trees (AU or dead) | at least one photo was taken | W | not yet | Y | NA | core
19|tree | consistency check | newly censused trees (AU or dead, with level selected for `wounded main stem`)| "wound" selected as FAD, AU or dead selected as status | W| 2021 | N | add wound to FAD list* | core
20|tree | consistency check | newly censused trees (AU or dead, with level selected for `canker,swelling,deformity`)| "canker" selected as FAD | W| 2021 | N | add canker to FAD list* | core
21|tree | consistency check | newly censused trees (AU or dead, with level selected for `rotting main stem`)| "rotting stem" or "hollow stem" selected as FAD| W| 2021 | N | add `rotting main stem` to FAD list* | core
22|tree | consistency check | newly censused trees (any FAD selected, or level selected for `canker,swelling,deformity`, `wounded main stem` , or `rotting main stem`)| status selected as AU or dead | W| 2021 | N | change live to AU | core
23|tree | consistency check - with previous | newly censused trees (A or AU) | tree was A or AU in previous year | W| 2021 | Y | NA| SCBI
24|tree | consistency check - with previous | newly censused trees (A or AU or DS) | tree was not DC in previous year | W| 2021 | Y | NA| SCBI
25|tree | completion check | newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI) | `Crown thinning`, `Epicormic growth`,  `D-shaped exit hole count`, `Crown position < 10 cm DBH` **(for stems <10cm)** all recorded | E | 2021 | Y | NA | SCBI
26|tree | consistency check | newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI) | if `Epicormic growth`>0, tree is AU | E | 2021 | N | set status to AU | SCBI
27|tree | consistency check | newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI) | if `Crown thinning`>1 , tree is AU or dead | E | 2021 |  sometimes | | SCBI
28|tree | consistency check | newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI) | if any EABF recorded, tree is AU or dead | E | 2021 | sometimes | --- | SCBI
29|tree | consistency check | newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI) | if `D-shaped exit hole count`>0, tree is AU or dead | E | 2021 | sometimes | --- | SCBI
30|tree | consistency check | newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI) | if tree is dead, `Epicormic growth`=0  | E | 2021 | sometimes | --- | SCBI
31|tree | consistency check | newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI) | if tree is dead, `Crown thinning`=5 | E | 2021 |  sometimes | Just the ones that are zero should be auto changed to 5.| SCBI



