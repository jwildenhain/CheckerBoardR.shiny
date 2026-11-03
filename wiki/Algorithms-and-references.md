# Algorithms and scientific references

## Common scoring convention

CheckerboardR represents response as fractional inhibition. For each dose pair it calculates a model-specific expected response and then:

```text
synergy score = observed inhibition - expected inhibition
```

Positive values therefore mean greater inhibition than the selected no-interaction model predicts; negative values mean less inhibition. A zero score means agreement with that model, not proof that the agents have no biological interaction.

## Highest Single Agent (HSA)

```text
expected = max(effect A, effect B)
```

HSA asks whether the combination outperforms the better monotherapy at the same doses. It is intuitive and conservative about clinical improvement, but it does not model probabilistic independence or dose equivalence.

Reference: Berenbaum MC. *What is synergy?* Pharmacological Reviews. 1989;41(2):93–141. [PubMed](https://pubmed.ncbi.nlm.nih.gov/2692037/).

## Bliss independence

```text
expected = A + B - A×B
```

Bliss treats the two effects as probabilistically independent. It is most natural when the agents can act through independent mechanisms and inhibition is expressed on a 0–1 scale.

Primary reference: Bliss CI. *The toxicity of poisons applied jointly.* Annals of Applied Biology. 1939;26:585–615. [doi:10.1111/j.1744-7348.1939.tb06990.x](https://doi.org/10.1111/j.1744-7348.1939.tb06990.x).

## Loewe additivity

Loewe uses dose additivity: a non-interacting combination behaves like a drug combined with itself. CheckerboardR numerically finds the effect satisfying the additivity isobole and reports observed minus expected inhibition. Loewe is most interpretable when the agents have comparable dose-response behaviour and each can produce the effect being evaluated.

Primary reference: Loewe S, Muischnek H. *Über Kombinationswirkungen. I. Mitteilung: Hilfsmittel der Fragestellung.* Naunyn-Schmiedebergs Archiv für Experimentelle Pathologie und Pharmakologie. 1926;114:313–326. [doi:10.1007/BF01952257](https://doi.org/10.1007/BF01952257).

## Zero Interaction Potency (ZIP)

ZIP compares changes in fitted dose-response potency while combining Bliss-like independence with dose-additivity concepts. CheckerboardR fits row-wise and column-wise combination responses, averages their predictions, and subtracts the zero-interaction expectation derived from the single-agent fits.

Primary reference: Yadav B, Wennerberg K, Aittokallio T, Tang J. *Searching for Drug Synergy in Complex Dose–Response Landscapes Using an Interaction Potency Model.* Computational and Structural Biotechnology Journal. 2015;13:504–513. [doi:10.1016/j.csbj.2015.09.001](https://doi.org/10.1016/j.csbj.2015.09.001).

## Conservative consensus

CheckerboardR defines the consensus reference as the cell-wise maximum of the HSA, Bliss and Loewe expected effects:

```text
consensus expected = max(HSA expected, Bliss expected, Loewe expected)
consensus score    = observed - consensus expected
```

This is a project-specific conservative summary: a combination must exceed the most demanding of the three reference effects at that dose pair. It is not a separately standardized pharmacological null model. Always inspect the component models as well as the consensus.

For a broader multi-model workflow, see Ianevski A et al. *SynergyFinder 2.0: visual analytics of multi-drug combination synergies.* NAR Genomics and Bioinformatics. 2020;2(1):lqaa010. [doi:10.1093/nargab/lqaa010](https://doi.org/10.1093/nargab/lqaa010).

## FICI

FICI is endpoint based rather than response-surface based. CheckerboardR finds the lowest tested single-agent concentrations reaching the selected inhibition endpoint, calculates ΣFIC for each qualifying combination well and reports the minimum.

The application uses conservative categories: synergy ≤0.5, antagonism >4 and indifference otherwise. Because MIC measurements vary by dilution and experimental conditions, report the endpoint, concentration units, tested range and replicate behaviour with every FICI result.

Reference: Odds FC. *Synergy, antagonism, and what the chequerboard puts between them.* Journal of Antimicrobial Chemotherapy. 2003;52(1):1. [doi:10.1093/jac/dkg301](https://doi.org/10.1093/jac/dkg301).

## Four-parameter logistic curves

The 4PL model estimates lower and upper asymptotes, midpoint concentration (IC50/EC50) and Hill slope. CheckerboardR uses these curves for continuous predictions required by Loewe and ZIP and displays the two marginal fits in the 1D plot.

References:

- Ritz C, Streibig JC. *Bioassay Analysis using R.* Journal of Statistical Software. 2005;12(5):1–22. [doi:10.18637/jss.v012.i05](https://doi.org/10.18637/jss.v012.i05).
- Yadav B et al. 2015, above, provides the 4PL parameterization in the context of drug-combination response surfaces.

## Cancer combination interpretation

Combination index methods and response-surface models are complementary, not interchangeable. For the mass-action/median-effect framework widely used in cancer pharmacology, see Chou TC. *Drug combination studies and their synergy quantification using the Chou–Talalay method.* Cancer Research. 2010;70(2):440–446. [doi:10.1158/0008-5472.CAN-09-1947](https://doi.org/10.1158/0008-5472.CAN-09-1947).

## Practical model selection

| Question | Useful starting point |
|---|---|
| Does the combination beat the better single agent? | HSA |
| Are the agents acting independently on a fractional response? | Bliss |
| Is the combination dose-equivalent to either agent alone? | Loewe |
| Does combination exposure change fitted potency across the surface? | ZIP |
| Do I need a stringent multi-model summary? | Consensus, followed by component-model inspection |
| Is this an MIC-style antimicrobial endpoint experiment? | FICI plus at least one response-surface model |

