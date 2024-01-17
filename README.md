# Should the choice of BOIN design parameter p.tox only depend on the target DLT rate?

# KEY POINTS 
## Question: 
When is setting the BOIN design parameter p.tox = 1.4 * target.DLT.rate not a great idea?

## Findings: 
When the early stopping parameter n.earlystop is relatively small or the cohortsize value is not optimized via simulation, it might be better to use p.tox < 1.4 * target.DLT.rate, or try out different cohortsize, or increase n.earlystop, whichever is both feasible and provides better operating characteristics. This is because if the cohortsize was not optimized via simulation, even when n.earlystop = 12 and cohortsize > 3, the BOIN escalation/de-escalation rules generated using p.tox = 1.4 * target.DLT.rate could be exactly the same as those calculated using p.tox > 3 * target.DLT.rate, which might not be acceptable for some pediatric trials targeting 10% DLT rate..

## Meaning: 
This study demonstrates the importance of interpreting BOIN design parameter p.tox as an interval of toxicity rates deemed overly toxic, rather than one prespecified value that corresponds to the lowest toxicity probability that is deemed overly toxic. When designing a dose-finding trial using BOIN, it is important to perform simulation studies to identify equivalent sets of BOIN design parameters that can generate the same boundary table so that we can better compare the safety properties of different boundary tables. 
