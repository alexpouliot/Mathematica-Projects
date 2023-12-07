(* ::Package:: *)

(* ::Section:: *)
(*BEGIN*)


BeginPackage["SineApprox`"];


sinWave::usage = "Simple sine wave function with arguments frequency,amplitude,phase,time";
sineApprox::usage = "Approximation of ordered pair data using sine functions with arguments
 data, max number of Sins in approximation. Option for 'Mode' can be specified in third 
position. Options for mode include 'Best'(default), 'Biggest', and 'Fewest'";


Begin["Private`"];


(* ::Section:: *)
(*Functions*)


sinWave[freq_,amp_,\[Phi]_,t_]:=amp Sin[2\[Pi] freq t+\[Phi]];


Options[sineApprox]={"Mode"->"Best"};
sineApprox[data_List,maxNoSins_Integer,OptionsPattern[]]:=Module[{originalLength,
timeStep,paddedLength,paddedTimeAmplitudes,fourierTransform,frequencyStep,
frequencySpace,powerSpectrum,totalPower,phaseCorrections,phaseSpectrum,
peakPositions,isolatedPeaks,noIsolatedPeaks,peakParameters,isolatedParameters,
nearestIsolatedPeakNo,groupedPeaks,nearPhasePowerFraction,bestPeaks,
orderedBestPeaks,modeList,peakList,subsetPower,powerCorrection,output},

(*get length and timestep from input data*)
originalLength=Length[data];
timeStep=Abs[Mean[Differences[data[[All,1]]]]];

(*get padded discrete fourier transform*)
paddedLength=2^(Ceiling[RealExponent[Max[originalLength],2]+1]);(*power of two is more efficient for FFT, extra power of two for good interpolation*)
paddedTimeAmplitudes=PadRight[data[[All,2]],paddedLength,0];
fourierTransform=Fourier[paddedTimeAmplitudes][[1;;Round[paddedLength/2]]];
frequencyStep=Abs[timeStep]^-1/paddedLength;
frequencySpace=frequencyStep*Range[0,Round[paddedLength/2]-1];

(*get power and phase spectra*)
powerSpectrum=(Re[#]^2+Im[#]^2)&/@fourierTransform;
totalPower=Total[powerSpectrum];

phaseCorrections=(Min[data[[All,1]]]/timeStep-1)timeStep 2\[Pi] frequencySpace;
(*phase offset when converting from freqency/time axis of data to frequency'/index
 space given by fourier transform*)
phaseSpectrum=Mod[ArcTan[Im[#],Re[#]]&/@fourierTransform-phaseCorrections,2\[Pi]]/.{x_?(#<=\[Pi]&):>x,x_?(#>\[Pi]&):>x-2\[Pi]};
(*corresponding phase OF A SINE WAVE AT EACH FREQUENCY(not the same as the phase 
of the fourier transform) in frequency basis defined by data, ReplaceAll rule just
 shifts things from -\[Pi] to \[Pi] instead of 0 to 2\[Pi], it doesn't change anything 
 practically*)

(*get most suitable peaks. 
Three options: 1)Biggest peaks in power spectrum ("Mode"->"Biggest")
2)Biggest Peak-find peaks in power spectrum (eliminates frequencies near peak)
("Mode"->"Fewest")
3)Combination: Compare peakfind peaks to nearby frequencies. Use peakfound peak if nearby 
powers with similar phase to peakfind peak constitute more than 80% of the power 
in the group, use all significant peaks in the region otherwise ("Mode"->"Best"/default)*)
If[OptionValue["Mode"]=="Best"||OptionValue["Mode"]=="Biggest",
peakPositions=
Sort[Transpose[{Range[Length[powerSpectrum]],powerSpectrum}],#1[[2]]>#2[[2]]&];
peakParameters=Transpose[{frequencySpace[[peakPositions[[All,1]]]],peakPositions[[All,2]],phaseSpectrum[[peakPositions[[All,1]]]]}];isolatedPeaks=DeleteCases[Sort[FindPeaks[powerSpectrum],#1[[2]]>#2[[2]]&],_?(#[[2]]<totalPower/1000&)];
];

If[OptionValue["Mode"]=="Best"||OptionValue["Mode"]=="Fewest",
isolatedPeaks=DeleteCases[Sort[FindPeaks[powerSpectrum],#1[[2]]>#2[[2]]&],_?(#[[2]]<totalPower/1000&)];
noIsolatedPeaks=Length[isolatedPeaks];
isolatedParameters=Transpose[{frequencySpace[[isolatedPeaks[[All,1]]]],isolatedPeaks[[All,2]],phaseSpectrum[[isolatedPeaks[[All,1]]]]}];
];

If[OptionValue["Mode"]=="Best",
nearestIsolatedPeakNo=Ordering[Abs[#-isolatedParameters[[All,1]]]][[1]]&/@peakParameters[[All,1]];
groupedPeaks=GatherBy[SortBy[Transpose[{peakParameters,nearestIsolatedPeakNo}],Last],Last][[All,All,1]];
nearPhasePowerFraction=Table[Total[Select[groupedPeaks[[i]],Abs[#[[3]]-isolatedParameters[[i,3]]]<=0.1&][[All,2]]]/Total[groupedPeaks[[i,All,2]]],{i,1,noIsolatedPeaks}];
bestPeaks=Table[If[nearPhasePowerFraction[[i]]>=0.8,{isolatedParameters[[i]]},Sort[groupedPeaks[[i]]],#1[[2]]>#2[[2]]&],{i,1,noIsolatedPeaks}];
orderedBestPeaks=Join[#[[1]]&/@bestPeaks,Flatten[Sort[Drop[#,1]&/@bestPeaks],1]];
];

modeList = Which[OptionValue["Mode"]=="Best",orderedBestPeaks,
OptionValue["Mode"]=="Biggest",peakParameters,
OptionValue["Mode"]=="Fewest",isolatedParameters];

peakList=If[Length[modeList]>maxNoSins,modeList[[;;maxNoSins]],modeList];
subsetPower=Total[peakList[[All,2]]];
powerCorrection=totalPower/subsetPower timeStep (2 originalLength)/paddedLength;

(*tabulate output parameters for Sine approximation *)
output=peakList/.{x_,y_,z_}:>{x,Sqrt[y powerCorrection],z}
]


End[];
EndPackage[];
