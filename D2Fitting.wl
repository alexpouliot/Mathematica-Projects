(* ::Package:: *)

BeginPackage["D2Fitting`"];


FrequencyFit::usage = "";
PeakEstimates::usage="";
ZeroCrossings::usage="";
Begin["Private`"]


Needs["RbLines`"]


ZeroCrossings[l_List]:=Flatten[Position[Sign[l[[1;;-2]]*l[[2;;]]],-1]];


PeakEstimates[data_,filterParam_,noPeaks_]:=Module[{smooth,firstDeriv,secondSmooth,
secondDeriv,potentialPeaks(*,thirdDeriv*),peakLocations},
smooth=GaussianFilter[data[[All,2]],filterParam];
firstDeriv=Differences[smooth]/Mean[Differences[data[[All,1]]]];
secondSmooth=GaussianFilter[firstDeriv,filterParam];
secondDeriv=Differences[secondSmooth]/Mean[Differences[data[[All,1]]]]^2;
potentialPeaks=ZeroCrossings[firstDeriv];
(*thirdDeriv=GaussianFilter[Differences[GaussianFilter[firstDeriv,filterParam/2],3],
filterParam/2];*)
peakLocations=potentialPeaks[[Ordering[secondDeriv[[potentialPeaks]],-noPeaks]]]
]


FrequencyFit[data_,filterParam_,cellLength_]:=Module[{locationEstimates,calGuess,slopeGuess,
offGuess,t0Guess,fit},
locationEstimates=PeakEstimates[data,filterParam,4];
calGuess=(6.51879 10^9)/(data[[Sort[locationEstimates][[-1]],1]]-data[[Sort[locationEstimates][[1]],1]]);
slopeGuess=(data[[-1,2]]-data[[1,2]])/(data[[-1,1]]-data[[1,1]]) 1/calGuess;
offGuess=data[[1,2]]- data[[1,1]] slopeGuess;
t0Guess=data[[locationEstimates[[-1]],1]];
fit=NonlinearModelFit[data,(-m x + b)Exp[-NatRbLines[nTot,cellLength,0,temp,0,0,
cal (t0-x)]+leak],{{m,slopeGuess},{b,offGuess},{nTot,1 10^16},{temp,295},
{cal,calGuess},{t0,t0Guess},{leak,0}},x]
]


End[];
EndPackage[];
