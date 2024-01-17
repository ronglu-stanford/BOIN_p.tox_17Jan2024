# Should the choice of BOIN design parameter p.tox only depend on the target DLT rate?

## ------------------- KEY POINTS 
## Question: 
When is setting the BOIN design parameter p.tox = 1.4 * target.DLT.rate not a great idea?

## Findings: 
When the early stopping parameter n.earlystop is relatively small or the cohortsize value is not optimized via simulation, it might be better to use p.tox < 1.4 * target.DLT.rate, or try out different cohortsize, or increase n.earlystop, whichever is both feasible and provides better operating characteristics. This is because if the cohortsize was not optimized via simulation, even when n.earlystop = 12 and cohortsize > 3, the BOIN escalation/de-escalation rules generated using p.tox = 1.4 * target.DLT.rate could be exactly the same as those calculated using p.tox > 3 * target.DLT.rate, which might not be acceptable for some pediatric trials targeting 10% DLT rate..

## Meaning: 
This study demonstrates the importance of interpreting BOIN design parameter p.tox as an interval of toxicity rates deemed overly toxic, rather than one prespecified value that corresponds to the lowest toxicity probability that is deemed overly toxic. When designing a dose-finding trial using BOIN, it is important to perform simulation studies to identify equivalent sets of BOIN design parameters that can generate the same boundary table so that we can better compare the safety properties of different boundary tables. 

## ------------------- ABSTRACT
## IMPORTANCE: 
On December 10, 2021, the FDA published a Determination Letter, along with a Statistical Review and Evaluation Report, and concluded that under the non-informative prior, the local Bayesian optimal interval design (BOIN) design, in its revised form, can be designated fit-for-purpose for identifying the maximum tolerated dose (MTD) of a new drug, assuming that dose-toxicity relationship is monotonically increasing. Although setting the BOIN design parameter p.tox = 1.4 * target.DLT.rate is recommended in almost all BOIN methodology articles and is the default value in the R package BOIN, it’s unclear if the choice of p.tox should only depend on the target DLT rate and whether certain range of p.tox could produce the same BOIN boundary table.

## DESIGN: 
In this simulation study, following parameters were varied one at a time, using R package BOIN, to explore each parameter’s effect on the equivalence intervals of p.saf and p.tox: 1) target DLT rate, 2) n.earlystop, 3) cutoff.eli, 4) cohortsize, and 5) ncohort. And a simple 3+3 design was used as an example to explore equivalent sets of BOIN design parameters that can generate the same boundary table.

## RESULTS: 
When the early stopping parameter n.earlystop is relatively small or the cohortsize value is not optimized via simulation, it might be better to use p.tox < 1.4 * target.DLT.rate, or try out different cohort sizes, or increase n.earlystop, whichever is both feasible and provides better operating characteristics. This is because if the cohortsize was not optimized via simulation, even when n.earlystop = 12 and cohortsize > 3, the BOIN escalation/de-escalation rules generated using p.tox = 1.4 * target.DLT.rate could be exactly the same as those calculated using p.tox > 3 * target.DLT.rate, which might not be acceptable for some pediatric trials targeting 10% DLT rate.

The traditional 3+3 design stops the dose finding process when 3 patients have been treated at the current dose level, 0 DLT has been observed, and the next higher dose has already been eliminated. If additional 3 patients were required to be treated at the current dose in the situation described above, the decision rules of this commonly used 3+3 design could be generated using BOIN design with target DLT rates ranging from 18% to 29%, p.saf ranging from 8% to 26%, and different p.tox values ranging from 39% to 99%. To generate this commonly used 3+3 design table, BOIN parameters also need to satisfy a set of conditions.

