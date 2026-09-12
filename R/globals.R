# globals.R
# The codebase uses the rlang `.data` pronoun for tidy-eval column references,
# so almost nothing bare needs declaring here. Keep this list minimal - add a
# name only when R CMD check actually reports "no visible binding for global
# variable" for it.

utils::globalVariables(c(
  ".data"   # rlang tidy-eval pronoun
))
