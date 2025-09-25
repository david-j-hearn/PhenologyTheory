nohup R --vanilla -e "source(\"validateParadox.R\"); run_AnalysisParadox(replicates = 1000, outputFile=\"output.redo.S1.txt\", scenarioNumber = 1, append=F, seed=1001)" > /dev/null 2> /dev/null &
nohup R --vanilla -e "source(\"validateParadox.R\"); run_AnalysisParadox(replicates = 1000, outputFile=\"output.redo.S2.txt\", scenarioNumber = 2, append=F, seed=2001)" > /dev/null 2> /dev/null &
nohup R --vanilla -e "source(\"validateParadox.R\"); run_AnalysisParadox(replicates = 1000, outputFile=\"output.redo.S3.txt\", scenarioNumber = 3, append=F, seed=3001)" > /dev/null 2> /dev/null &
nohup R --vanilla -e "source(\"validateParadox.R\"); run_AnalysisParadox(replicates = 1000, outputFile=\"output.redo.S4.txt\", scenarioNumber = 4, append=F, seed=4001)" > /dev/null 2> /dev/null &
nohup R --vanilla -e "source(\"validateParadox.R\"); run_AnalysisParadox(replicates = 1000, outputFile=\"output.redo.S5.txt\", scenarioNumber = 5, append=F, seed=5001)" > /dev/null 2> /dev/null &
nohup R --vanilla -e "source(\"validateParadox.R\"); run_AnalysisParadox(replicates = 1000, outputFile=\"output.redo.S6.txt\", scenarioNumber = 6, append=F, seed=6001)" > /dev/null 2> /dev/null &

#source("validateParadox.TakeII.R"); run_AnalysisParadox(replicates = 1000, outputFile="output.X.1.txt", scenarioNumber = 1, append=F, seed=1001)
#source("validateParadox.TakeII.R"); run_AnalysisParadox(replicates = 1000, outputFile="output.X.2.txt", scenarioNumber = 2, append=F, seed=2001)
#source("validateParadox.TakeII.R"); run_AnalysisParadox(replicates = 1000, outputFile="output.X.3.txt", scenarioNumber = 3, append=F, seed=3001)
#source("validateParadox.TakeII.R"); run_AnalysisParadox(replicates = 1000, outputFile="output.X.4.txt", scenarioNumber = 4, append=F, seed=4001)
#source("validateParadox.TakeII.R"); run_AnalysisParadox(replicates = 1000, outputFile="output.X.5.txt", scenarioNumber = 5, append=F, seed=5001)
#source("validateParadox.TakeII.R"); run_AnalysisParadox(replicates = 1000, outputFile="output.X.6.txt", scenarioNumber = 6, append=F, seed=6001)
