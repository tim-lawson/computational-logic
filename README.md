# Computational Logic

This repository holds the code and report written to fulfil the coursework requirements for the unit
[Computational Logic for Artificial Intelligence](https://www.bris.ac.uk/unit-programme-catalogue/UnitDetails.jsa?ayrCode=23%2F24&unitCode=COMSM0022)
at the University of Bristol.
It is based on the repository [simply-logical/ComputationalLogic](https://github.com/simply-logical/ComputationalLogic).
We introduced several features:

- The _modus tollens_ inference rule
- (Exclusive) disjunction
- Default rules
- Existential quantification
- Negative, conjunctive, and disjunctive facts

These are demonstrated by the Jupyter Notebook [demo.ipynb](/demo.ipynb) and unit tests [cli_test.py](prolog/cli_test.py).

## Installation

[SWI-Prolog](https://www.swi-prolog.org/) must be installed.
For example, on macOS, you can [SWI-Prolog with Homebrew](https://formulae.brew.sh/formula/swi-prolog):

```sh
brew install swi-prolog
```

In this case, the executable path for the `arthurwang.vsc-prolog` extension should be set to `/opt/homebrew/bin/swipl`:

```json
{
  "prolog.executablePath": "/opt/homebrew/bin/swipl"
}
```

## Testing

Python must be installed.
Create a virtual environment, install the dependencies in `requirements.txt`, and execute:

```sh
cd prolog
swipl --goal=cli -o cli -c cli.pl
pytest
```
