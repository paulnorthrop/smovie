Please may I submit a patch release to fix the installation ERRORs of smovie v1.0.0 on r-oldrel-osx and r-release-osx?

The ERRORs stem from the examples and seem to occur because BWidget (a system requirement for rpanel on which smovie depends) is not available.  I now check that BWidget is available to avoid throwing an error when the examples are run.  I have also declared BWidget explicitly in SystemRequirements: in DESCRIPTION and made a note to users in the README file.

Please accept my apologies: I should have anticipated this.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

- ubuntu 12.04 (on travis-ci), R-release, R-devel    
- osx (on travis-ci), R-oldrel, R-release            
- win-builder (R-devel and R-release)

## Downstream dependencies

This is a new submission
