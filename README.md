Models, analysis, and reproducible results for the paper ["Modeling the effects of crime type and evidence on judgments about guilt"](https://link.tbd).

# What you need (dependencies):
We used R via [RStudio](https://www.rstudio.com/). RStudio is not strictly necessary, but it may make building some aspects of the project (e.g., the supplement) easier. We also make heavy use of the [Stan](http://mc-stan.org/) probabilistic programming language and the [tidyverse](https://tidyverse.tidyverse.org/index.html). A few other dependencies are used for particular plots or tables.

In particular, we use:
- `rmarkdown` (plus dependencies)
- `rstan` (plus dependencies)
- `tidyverse`
- `kableExtra`
- `magick`
- `gridBase`
- `Hmisc`

# About the data
The data are recorded in a single file, `combined_data.csv` in the `data` folder. The file is a single table, one line per rating given, with the following columns:
- `uid`: Unique id for each participant.
- `scenario`: Integer indicating which crime was presented on a given trial: (1 - 33).
- `physical`: Which physical evidence was presented? (`No Physical`, `Non-DNA`, `DNA`)
- `history`: What criminal history information was presented? (`No History`, `Unrelated`, `Related`)
- `witness`: What eyewitness information was presented? (`No Witness`, `Yes Witness`)
- `nonwhite`: Did the participant identify as non-white? (`FALSE`, `TRUE`)
- `hispanic`: Did the participant identify as hispanic? (`FALSE`, `TRUE`)
- `female`: Did the participant identify as female? (`FALSE`, `TRUE`)
- `question`: In what order did the participant encounter this case to be rated? (0 - 32)
- `evidence_shown`: Was any evidence shown? Some participants performed a version of the task with no evidence.
- `guilty`: Did the participant think the defendant was guilty?
- `age`: Participant's reported age.
- `gender`: Participant's reported gender. Recoded as `female` for modeling.
- `race`: Participant's reported race. Binarized as `nonwhite` for modeling.
- `ethnicity`: Participant's reported ethnicity. Binarized as `hispanic` for modeling.
- `education`: Participant's highest level of education.
- `political_party`: Participant's party affiliation.
- `rating_type`: Which type of rating does the datum represent:
  - `rating`: Most common. "How strong is the case that the accused committed this crime?"
  - `rate_punishment`: Next most common when participants gave two or more ratings. "How severe should the punishment be for someone who commits a crime like this one?"
  - `rate_outrage`: "When you read about crimes like this one, how upset to you feel?"
  - `rate_threat`: "How likely is this crime to occur in your community?"
  - `rate_threat2`: "When you read about a crime like this, how concerned do you feel for your own safety, or the safety of your community?"
- `rating`: Numerical rating for the relevant question (1 - 100).
- `group`: Which experimental group the participant belonged to:
  - `mturk`: Amazon Mechanical Turk sample.
  - `legal`: Law students
  - `ilsa`: Illinois Prosecutors
  - `lsba`: Louisiana Bar

All together, the data comprise more than 144,000 ratings from 878 unique individuals. Demographic information were collected from all participants but only included in the combined data for the mTurk sample.

# Building the project
The main directory contains a Makefile. Mac and Linux users should have `make` installed. Windows users will need to get `make` ([Rtools](https://cran.r-project.org/bin/windows/Rtools/) has it). See also this [StackOverflow Answer](https://stackoverflow.com/questions/33608345/how-to-execute-a-makefile-from-r).

Once you have `make` installed, you can open a terminal, navigate to the project directory, and then type
```shell
$ make models
```
to run all the models and postprocess their outputs,
```shell
$ make figs
```
to make all the figures,
or
```shell
$ make supplement
```
to generate the supplement (from an RMarkdown file). The latter two require that the postprocessed model outputs exist. So the models will be run in any case.

Finally, if you want to make everything, just do
```shell
$ make
```

**Warning:** The Stan models can be time-consuming to run. On a four-core machine with a decent processor, the entire set of models produces a couple hundred MB of outputs and requires over **14 hours** to finish. If you're on a laptop that's put to sleep, Stan should resume when the laptop wakes up, but the whole process is best done on a desktop that can be left alone for the better part of a day.
