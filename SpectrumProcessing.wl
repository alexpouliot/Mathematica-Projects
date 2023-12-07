(* ::Package:: *)

(* ::Section:: *)
(*Preamble*)


BeginPackage["SpectrumProcessing`"];

importPicoData::usage = "importPicoData[filename] opens picoscope data
in filename and saves the average period, time step, averaged frequency, power
and spectrum plots respectively, all with time as independant variable, 
and all spectrum data";
binarizeData::usage = "binarizeData[directory], 
exports picoscope data in directory as lightweight binary data";
importAvgBinaryData::usage = "importAvgBinaryData[directory] imports the averages
and filenames in the directory";
binaryReadNamesFile::usage = "binaryReadNamesFile[binaryFolder] reads the names file 
in the current directory and specified folder";
importBinaryRawData::usage="importBinaryRawData[directory,filename] imports
the binary data file specified in the directory given";


Begin["Private`"];


(* ::Section:: *)
(*Public Functions*)


importPicoData[filename_]:=Module[{rawDat,timeDat,specDat,powDat,freqDat,
trigDat,nAvgs,posTriggers,negTriggers,trigsPerTrace,allTriggers,avgVerts,avgPeriod,
dt,avgPointPeriod,avgFreq,avgTime,avgPow,avgSpec,dataLength,freqTime,powTime,specTime},
rawDat=ToExpression[Import[#,"CSV"][[4;;]]]&/@filename;
timeDat=rawDat[[All,All,1]];
specDat=rawDat[[All,All,2]];
powDat=rawDat[[All,All,3]];
freqDat=rawDat[[All,All,4]];
trigDat=rawDat[[All,All,5]];

(*Getperiod by average time between triggers*)

nAvgs=Length[trigDat];

posTriggers=Flatten[Position[Differences[trigDat[[#]]],_?(#>1&)]]&/@Range[nAvgs]; 
negTriggers=Flatten[Position[Differences[trigDat[[#]]],_?(#<-1&)]]&/@Range[nAvgs];
trigsPerTrace=Floor[Mean[Length[#]&/@posTriggers]+Mean[Length[#]&/@negTriggers]];
allTriggers=Sort[#]&/@ArrayReshape[Transpose[{posTriggers[[All,1;;2]],negTriggers[[All,1;;2]]}],{nAvgs,trigsPerTrace}];
avgVerts=Mean[N[allTriggers]];

(*get avg period from triggers*)
avgPointPeriod=avgVerts[[4]]-avgVerts[[2]];
dt = timeDat[[1,2]]-timeDat[[1,1]];
avgPeriod=dt*avgPointPeriod;

(*collect data for export*)
avgFreq=Mean[freqDat];
avgTime=Mean[timeDat];
avgPow=Mean[powDat];
avgSpec=Mean[specDat];
freqTime=Transpose[{avgTime,avgFreq}];
powTime=Transpose[{avgTime,avgPow}];
specTime=Transpose[{avgTime,avgSpec}];

{avgPeriod,freqTime,powTime,specTime,specDat}
]


binarizeData[directory_]:=Module[{allFolders,folders,dataFiles,
nFolders,binaryFolder,namesFile,periodFile,freqFile,powFile,specFile,
folderName,importData,trimmedFiles},

SetDirectory[ParentDirectory[directory]];
folderName=StringSplit[directory,{"/","\\"}][[-1]];
$HistoryLength=0;
allFolders=FileNames[All,directory];
folders=DeleteCases[allFolders,_?(StringMatchQ["*Processed"|"*backgrounds"|"*ProcessedBinary"])];
dataFiles=FileNames["*.csv",#]&/@folders;
nFolders=Length[folders];

binaryFolder="ProcessedBinary/"<>folderName<>"/";
If[!FileExistsQ[StringDrop[binaryFolder,-1]],CreateDirectory[StringDrop[binaryFolder,-1]]];

(*Write binary Filenames*)
trimmedFiles=StringDelete[#,directory<>"/"]&/@dataFiles;
writeBinaryFilenames[trimmedFiles,binaryFolder];

(*Initialize Files*)

periodFile=binaryFolder<>"avgPeriod.dat";
freqFile=binaryFolder<>"freqTime.dat";
powFile=binaryFolder<>"powTime.dat";
specFile=binaryFolder<>"specTime.dat";

Do[
importData=importPicoData[dataFiles[[oo]]];

(*write period to period File*)
BinaryWrite[periodFile,importData[[1]],"Real64"];

(*write freq-time avg to freq File*)
writeAvgData[importData[[2]],freqFile];

(*write pow-time avg to pow File*)
writeAvgData[importData[[3]],powFile];

(*write spec-time avg to spec File*)
writeAvgData[importData[[4]],specFile];

writeAllData[importData[[5]],trimmedFiles[[oo]],binaryFolder];

,{oo,1,nFolders}];

Close[#]&/@{periodFile,freqFile,powFile,specFile};

]


importAvgBinaryData[directory_]:=Module[{fileNames,
nFolders,avgPeriod,freqTime,powTime,specTime},

SetDirectory[directory];

fileNames=binaryReadNamesFile[];

nFolders=Length[fileNames];

avgPeriod=BinaryReadList["avgPeriod.dat","Real64"];

freqTime=binaryReadAvgs["freqTime.dat",nFolders];

powTime=binaryReadAvgs["powTime.dat",nFolders];

specTime=binaryReadAvgs["specTime.dat",nFolders];

{fileNames,avgPeriod,freqTime,powTime,specTime}
]


binaryReadNamesFile[]:=Module[{binaryFolder,namesDir,nFolders,names,
nAvgs,nameLength},

namesDir=OpenRead["fileNames.dat",BinaryFormat->True];
nFolders=BinaryRead[namesDir,"UnsignedInteger16"];

names=Table[{},{i,1,nFolders}];

Do[
nAvgs=BinaryRead[namesDir,"UnsignedInteger16"];
names[[i]]=Table[{},{j,1,nAvgs}];
Do[
nameLength=BinaryRead[namesDir,"UnsignedInteger16"];
names[[i,j]]=
StringJoin[BinaryReadList[namesDir,"Character8",nameLength]];
,{j,1,nAvgs}]
,{i,1,nFolders}];
Close[namesDir];
names
]


importBinaryRawData[directory_,fileName_]:=Module[{fileStream,
binaryName,nPoints,rawData},
SetDirectory[directory<>"/binarySpectrumData"];

binaryName=StringDrop[fileName,-3]<>"dat";
fileStream=OpenRead[binaryName,BinaryFormat->True];
nPoints=BinaryRead[fileStream,"UnsignedInteger32"];
rawData=BinaryReadList[fileStream,"Real64",nPoints];
Close[fileStream];

rawData
]


(* ::Section:: *)
(*Private Functions*)


writeBinaryFilenames[files_,binaryFolder_]:=Module[{namesFile},
namesFile=binaryFolder<>"fileNames.dat";

BinaryWrite[namesFile,Length[files],"UnsignedInteger16"];
Do[
BinaryWrite[namesFile,Length[files[[i]]],"UnsignedInteger16"];
Do[
BinaryWrite[namesFile,StringLength[files[[i,j]]],"UnsignedInteger16"];
BinaryWrite[namesFile,files[[i,j]],"Character8"];
,{j,1,Length[files[[i]]]}];
,{i,1,Length[files]}];
Close[namesFile];
]


writeAvgData[avgData_,fileName_]:=Module[{},
BinaryWrite[fileName,Length[Flatten[avgData]],"UnsignedInteger32"];
BinaryWrite[fileName,Flatten[avgData],"Real64"];
]


writeAllData[allData_,fileNames_,binaryFolder_]:=Module[{dataDirectory,
nFiles,dataFile,subDirLists,dirDepth,joinLocs,listWJoiners,subDirs},

(*create main directory*)
dataDirectory=binaryFolder<>"binarySpectrumData/";
If[!FileExistsQ[StringDrop[dataDirectory,-1]],
CreateDirectory[binaryFolder<>"binarySpectrumData"]];

(*create subdirectories*)
nFiles=Length[fileNames];
subDirLists=StringSplit[fileNames,{"/","\\"}];
dirDepth=Length[subDirLists[[1]]]-1;
joinLocs=Partition[Range[2,dirDepth,1],1];
listWJoiners=Insert[#,"/",joinLocs]&/@subDirLists[[All,1;;dirDepth]];
subDirs=dataDirectory<>#&/@DeleteDuplicates[StringJoin[#]&/@listWJoiners];

If[!FileExistsQ[#],CreateDirectory[#]]&/@subDirs;

Do[
dataFile=dataDirectory<>StringDrop[fileNames[[i]],-3]<>"dat";
BinaryWrite[dataFile,Length[allData[[i]]],"UnsignedInteger32"];
BinaryWrite[dataFile,allData[[i]],"Real64"];
Close[dataFile];
,{i,1,nFiles}]
]


binaryReadAvgs[filePath_,nFolders_]:=Module[{dir,avgs,flatLength,
flatAvg},

dir=OpenRead[filePath,BinaryFormat->True];
avgs=Table[{},{i,1,nFolders}];
Do[
flatLength=BinaryRead[dir,"UnsignedInteger32"];
flatAvg=BinaryReadList[dir,"Real64",flatLength];
avgs[[i]]=Partition[flatAvg,2];
,{i,1,nFolders}];
Close[dir];
avgs
]




(* ::Section:: *)
(*End*)


End[];
EndPackage[];
