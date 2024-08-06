\encoding{UTF-8}


\name{RunModel_CemaNeigeGR4J_Glacier}
\alias{RunModel_CemaNeigeGR4J_Glacier}


\title{Run with the RunModel_CemaNeigeGR4J_Glacier hydrological model}


\description{
This function runs the RunModel_CemaNeigeGR4J_Glacier daily lumped model over the test period. The model combines the GR4J rainfall-runoff model and the CemaNeige snow model, with an additional glacier module using the temperature melt index (TMI) model described below. The glacier module is spatially discretized in the same way as the CemaNeige layers.}


\usage{
RunModel_CemaNeigeGR4J_Glacier(InputsModel, RunOptions, Param)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 6 (or 8 parameters if \code{IsHyst = TRUE}, see \code{\link{CreateRunOptions}} for details)
  \tabular{ll}{
    GR4J X1         \tab production store capacity [mm]                                          \cr
    GR4J X2         \tab intercatchment exchange coefficient [mm/d]                              \cr
    GR4J X3         \tab routing store capacity [mm]                                             \cr
    GR4J X4         \tab unit hydrograph time constant [d]                                       \cr
    CemaNeige X1   \tab weighting coefficient for snow pack thermal state [-]                   \cr
    CemaNeige X2   \tab degree-day melt coefficient [mm/°C/d]                                   \cr
    Glacier Fi      \tab ice melting factor [mm/°C/d]                                            \cr
    Glacier Tm      \tab ice melting threshold temperature [°C]                                  \cr
    Glacier SWEth   \tab threshold amount of SWE on the glacier before ice melt occurs [mm]      \cr
  }}
}


\value{
[list] containing the function outputs organised as follows:
  \tabular{ll}{
    \emph{$DatesR  } \tab [POSIXlt] series of dates                                                   \cr
    \emph{$PotEvap } \tab [numeric] series of input potential evapotranspiration [mm/d] (E)           \cr
    \emph{$Precip  } \tab [numeric] series of input total precipitation (P) [mm/d]                    \cr
    \emph{$Prod    } \tab [numeric] series of production store level (S) [mm]                         \cr
    \emph{$Pn      } \tab [numeric] series of net rainfall (Pn) [mm/d]                                \cr
    \emph{$Ps      } \tab [numeric] series of the part of Pn filling the production store (Ps) [mm/d] \cr
    \emph{$AE      } \tab [numeric] series of actual evapotranspiration [mm/d]                        \cr
    \emph{$Perc    } \tab [numeric] series of percolation (Perc) [mm/d]                               \cr
    \emph{$PR      } \tab [numeric] series of Pr=Pn-Ps+Perc (Pr) [mm/d]                               \cr
    \emph{$Q9      } \tab [numeric] series of UH1 outflow (Q9) [mm/d]                                 \cr
    \emph{$Q1      } \tab [numeric] series of UH2 outflow (Q1) [mm/d]                                 \cr
    \emph{$Rout    } \tab [numeric] series of routing store level (R1) [mm]                           \cr
    \emph{$Exch    } \tab [numeric] series of potential semi-exchange between catchments [mm/d]       \cr
    \emph{$AExch1  } \tab [numeric] series of actual exchange between catchments for branch 1 [mm/d]  \cr
    \emph{$AExch2  } \tab [numeric] series of actual exchange between catchments for branch 2 [mm/d]  \cr
    \emph{$AExch   } \tab [numeric] series of actual exchange between catchments (1+2) [mm/d]         \cr
    \emph{$QR      } \tab [numeric] series of routing store outflow (Qr) [mm/d]                       \cr
    \emph{$QD      } \tab [numeric] series of direct flow from UH2 after exchange (Qd) [mm/d]         \cr
    \emph{$Qsim    } \tab [numeric] series of simulated discharge (Q) [mm/d]                          \cr
    \emph{$CemaNeigeLayers} \tab [list] list of CemaNeige outputs (1 list per layer)                  \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Pliq         } \tab [numeric] series of liquid precip. [mm/d]                    \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Psol         } \tab [numeric] series of solid precip. [mm/d]                     \cr
    \emph{$CemaNeigeLayers[[iLayer]]$SnowPack     } \tab [numeric] series of snow pack (snow water equivalent) [mm]   \cr
    \emph{$CemaNeigeLayers[[iLayer]]$ThermalState } \tab [numeric] series of snow pack thermal state [°C]             \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Gratio       } \tab [numeric] series of Gratio [0-1]                             \cr
    \emph{$CemaNeigeLayers[[iLayer]]$PotMelt      } \tab [numeric] series of potential snow melt [mm/d]               \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Melt         } \tab [numeric] series of actual snow melt [mm/d]                  \cr
    \emph{$CemaNeigeLayers[[iLayer]]$PliqAndMelt  } \tab [numeric] series of liquid precip. + actual snow melt [mm/d] \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Temp         } \tab [numeric] series of air temperature [°C]                     \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Gthreshold   } \tab [numeric] series of melt threshold [mm]                      \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Glocalmax    } \tab [numeric] series of local melt threshold for hysteresis [mm] \cr
    \emph{RunOptions$WarmUpQsim} \tab [numeric] series of simulated discharge (Q) on the warm-up period [mm/d] \cr
    \emph{RunOptions$Param     } \tab [numeric] parameter set parameter set used by the model                  \cr
    \emph{$StateEnd} \tab [numeric] states at the end of the run: store & unit hydrographs levels [mm], CemaNeige states [mm & °C]. See \code{\link{CreateIniStates}} for more details \cr
    \emph{TotGlacMelt} \tab [numeric] modelled glacier melt Mi as total over all elevation bands [mm/d] \cr
  }
Refer to the provided references or to the package source code for further details on these model outputs.
}


\details{
Glacier melt in the elevation layers is given as liquid input into the GR4J model, similar to snow melt. SWE is modeled within the CemaNeige compartment and given as input to the glacier module. Ice melt starts only if the temperature and SWE thresholds are exceeded. For calculating Ai, please refer to \code{\link{CreateRunOptions}}. The formula for calculating the daily ice melt, \code{Mi}, from glaciers is given by:
  \deqn{M_i = \left\{
    \begin{array}{ll}
      F_i \times (T - T_m) \times A_i & \text{if } SWE \geq SWE_{th} \text{ and } T \geq T_m \\
      0 & \text{if } SWE < SWE_{th} \text{ or } T < T_m
    \end{array}
  \right.}{M_i = {F_i*(T-T_m)*A_i   if SWE ≥ SWE_th  and T ≥ T_m
0                                   if SWE < SWE_th  and T < T_m }}

\cr
\cr
\if{html}{\figure{diagramGR4JGlacier-EN.png}{options: width="80\%" alt="Figure: diagramGR46JGlacier-EN.png"}}
\if{latex}{\figure{diagramGR4JGlacier-EN.pdf}{options: width=6cm}}

The choice of the CemaNeige version is explained in \code{\link{CreateRunOptions}}. \cr
For further details on the model, see the references section. \cr
For further details on the argument structures and initialisation options, see \code{\link{CreateRunOptions}}.
\cr
\cr

}




\examples{
library(airGR)

## loading catchment data
data(L0123002)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR4J, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E, TempMean = BasinObs$T,
                                 ZInputs = median(BasinInfo$HypsoData),
                                 HypsoData = BasinInfo$HypsoData, NLayers = 5)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1999-12-31"))


## --- original version of CemaNeige

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J, InputsModel = InputsModel,
                               IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 408.774, X2 = 2.646, X3 = 131.264, X4 = 1.174,
           CNX1 = 0.962, CNX2 = 2.249)
OutputsModel <- RunModel_CemaNeigeGR4J(InputsModel = InputsModel,
                                       RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)


## --- version of CemaNeige with the Linear Hysteresis

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J, InputsModel = InputsModel,
                               IndPeriod_Run = Ind_Run, IsHyst = TRUE)

## simulation
Param <- c(X1 = 408.774, X2 = 2.646, X3 = 131.264, X4 = 1.174,
           CNX1 = 0.962, CNX2 = 2.249, CNX3 = 100, CNX4 = 0.4)
OutputsModel <- RunModel_CemaNeigeGR4J(InputsModel = InputsModel,
                                       RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Claude Michel, Charles Perrin, Audrey Valéry, Vazken Andréassian, Olivier Delaigue, Guillaume Thirel
}


\references{
Perrin, C., Michel, C. and Andréassian, V. (2003).
  Improvement of a parsimonious model for streamflow simulation.
  Journal of Hydrology, 279(1-4), 275-289, \doi{10.1016/S0022-1694(03)00225-7}.
\cr\cr
Riboust, P., Thirel, G., Le Moine, N. and Ribstein, P. (2019).
  Revisiting a simple degree-day model for integrating satellite data: Implementation of SWE-SCA hystereses.
  Journal of Hydrology and Hydromechanics, 67(1), 70–81, \doi{10.2478/johh-2018-0004}.
\cr\cr
Valéry, A., Andréassian, V. and Perrin, C. (2014).
  "As simple as possible but not simpler": What is useful in a temperature-based snow-accounting routine?
  Part 1 - Comparison of six snow accounting routines on 380 catchments.
  Journal of Hydrology, 517(0), 1166-1175, \doi{10.1016/j.jhydrol.2014.04.059}.
\cr\cr
Valéry, A., Andréassian, V. and Perrin, C. (2014).
  "As simple as possible but not simpler": What is useful in a temperature-based snow-accounting routine?
  Part 2 - Sensitivity analysis of the Cemaneige snow accounting routine on 380 catchments.
  Journal of Hydrology, 517(0), 1176-1187, \doi{10.1016/j.jhydrol.2014.04.058}.
}


\seealso{
\code{\link{RunModel_CemaNeige}}, \code{\link{RunModel_CemaNeigeGR5J}},
\code{\link{RunModel_CemaNeigeGR6J}}, \code{\link{RunModel_GR4J}},
\code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateIniStates}}.
}

