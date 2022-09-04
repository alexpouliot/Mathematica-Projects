(* ::Package:: *)

BeginPackage["TDMS`"];


TDMSContents::usage = "TDMSContents[file] reads group and channel names of the TDMS file at the specified path. Returns directory as an association withe the following structure, <|group1->{channel1,channel2,...},group2->{channel1,...}...|>"; 


TDMSLoad::usage = "TDMSLoad[file] saves ALL metadata for ALL groups and channels in the file. Equivalent to running TDMSProperties for every channel in the file. This will allow fast data retrieval afterwards but will generally be slower than looking up individual channels. Use if you need to acess all data in the file anyway, or if data is in a large, defragmented file.";


TDMSImport::usage = "TDMSImport[file,group,channel] imports data from the specified channel within the TDMS file at the specified path. Will use existing channel properties if available, will find the properties itslef if not, therefore TDMSProperties does not need to be called by the user.";


TDMSProperties::usage = "TDMSProperties[{file,group,channel}] reads properties associated with the specified object (file, group or channel) within the TDMS file. If no channel is provided, reads properties of the group, if no channel and group provided, reads properties of the file. Interprets the metadata variable $TDMSInfo[file], if the properties have already been saved, locates and saves them if they have not been read yet.";


$TDMSInfo::usage = "All file metadata will be recorded on $TDMSInfo[file] so that it can be accessed between functions in this package. Information is stored as an association with the following format <|''Directory''-><|ContentsDirectory (see TDMSContents)|>,object1adress->object1properties,object2address->object2properties...|>, where object adresses are strings, '/' for the file, '/groupname' for groups and '/groupname/channelname' for channels. objectproperties, lists each property in an association, <|property1->property1value,...|>. This is useful for debugging purposes but normally this information should be accessed through TDMSProperties";


debugPrint::usage = "Hides error reports which can be revealed when debugging";


Begin["Private`"];


(* ::Text:: *)
(*Known deficienices: String data, interleaved data, DAQmx data, fixedpoint data, unitful data*)


(* ::Section:: *)
(*PUBLIC FUNCTIONS*)


(*TDMSLoad[file] reads all metadata in the file and saves info to an association file in memory with variable name 
$TDMSIndex[file] so that it can be acessed by other functions to read the file. Returns the /group/channel list, 
which should be all you need as a user*)

TDMSLoad[file_String]:=Module[{metaFile,dataFilePosition,segmentLength},

(*Check if the file provided can be opened and is a tdms file*)
If[tdmsOpen[file],
	Return["TDMSLoad: Unable to open file"]];

(*Check if the file specified has an associated tdms_index file which can be opened, use it to access metadata
going forward if it does*)
If[checkForIndex[file],
	If[!tdmsOpen[file<>"_index"],{
		debugPrint["TDMSLoad: index file found!"];
		metaFile=file<>"_index";},{
		debugPrint["TDMSLoad: index file invalid, using original file instead"];
		metaFile=file}],{
	debugPrint["TDMSLoad: no index file found, using original file instead"];
	metaFile=file}];

(*Creat global variable to store metadata, creat empty directory path*)
$TDMSInfo[file]=Association["Directory"-><||>];	

(*keep track of the position in the data file, regardless of whether we ae actually reading metadata out of the data file or an index file*)
dataFilePosition=0;
(*Read metadata*)
debugPrint[StreamPosition[metaFile]];
While[!tdmsCheck[metaFile],{
	segmentLength=segmentRead[metaFile,dataFilePosition];
	dataFilePosition=dataFilePosition+segmentLength+28;
	debugPrint[segmentLength]}];

If[metaFile=!=file,
	Close[metaFile]];
Close[file];

$TDMSInfo[file]["Directory"]

];			


TDMSContents[file_String]:=Module[{metaFile},

(*Check if the file provided can be opened and is a tdms file*)
If[tdmsOpen[file],
	Return["TDMSContents: Unable to open file"]];

(*Check if the file specified has an associated tdms_index file which can be opened, use it to access metadata
going forward if it does*)
If[checkForIndex[file],
	If[!tdmsOpen[file<>"_index"],{
		debugPrint["TDMSContents: index file found!"];
		metaFile=file<>"_index";},{
		debugPrint["TDMSContents: index file invalid, using original file instead"];
		metaFile=file;}],{
	debugPrint["TDMSContents: no index file found, using original file instead"];
	metaFile=file}];

(*Check if info variable already created, create it if it hasnt been and start new Directory*)
If[!AssociationQ[$TDMSInfo[file]],
	$TDMSInfo[file]=<|"Directory"-><||>|>,
	AssociateTo[$TDMSInfo[file],"Directory"-><||>]];

While[!tdmsCheck[metaFile],
	takeNames[metaFile]];

If[metaFile=!=file,
	Close[metaFile]];
Close[file];
$TDMSInfo[file]["Directory"]]


(*TDMSImport[file,group,channel] imports data from the specified channel within the TDMS file at the specified path.
Data is only read when this function is called*)

TDMSImport[file_String,group_String,channel_String]:=Module[{len,emptyPaths,findList,groupPath,channelPath,channelInfo,raw,scaled,scaling,timeseries,byteOrder,out},


(*Check if the file provided can be opened and is a tdms file*)
If[tdmsOpen[file],
	Return["TDMSImport: Unable to open file"]];

(*generate keys for group and channel*)
groupPath=tdmsPathWrite[{group}];
channelPath=tdmsPathWrite[{group,channel}];

(*check for metadata, prepare to create some if none exists*)
If[!AssociationQ[$TDMSInfo[file]],
	$TDMSInfo[file]=<|Directory-><||>|>];

(*search for metadata for the file group and channel*)
(*first look for file data*)
findList={};
If[!KeyExistsQ[$TDMSInfo[file],#],
	AppendTo[findList,#]]&/@{"/",groupPath,channelPath};

If[Length[findList]>0,
	tdmsGrab[file,findList]];
	If[AnyTrue[{"/",groupPath,channelPath},!KeyExistsQ[$TDMSInfo[file],#]&],
		Return["TDMSImport: Cannot find data at "<>file<>": /"<>group<>"/"<>channel]];

channelInfo=Join[$TDMSInfo[file]["/"],$TDMSInfo[file][groupPath],$TDMSInfo[file][channelPath]];

SetStreamPosition[file,channelInfo["DataStart"]];

(*check if data type is variable length (i.e. strings) and import*)
If[channelInfo["Type"]==="String",{
	out=typeRead[file,channelInfo["Size"],channelInfo["BigEndian"]];
	Return[out]}];

If[channelInfo["BigEndian"],
	byteOrder=1;,
	byteOrder=-1];
	
(*import raw data*)
len=channelInfo["Length"];
raw=typeRead[file,channelInfo["Type"],channelInfo["Length"],byteOrder];

(*scale if necessary*)
If[AllTrue[{"NI_Scaling_Status","NI_Scale[0]_Linear_Slope","NI_Scale[0]_Linear_Y_Intercept"},KeyExistsQ[channelInfo,#]&],
	If[channelInfo["NI_Scaling_Status"]==="unscaled",
		scaled=channelInfo["NI_Scale[0]_Linear_Slope"]#+channelInfo["NI_Scale[0]_Linear_Y_Intercept"]&/@raw,
		scaled=raw],
	scaled=raw];
debugPrint[scaled];

If[KeyExistsQ[channelInfo,"wf_increment"],{
	timeseries=Range[0,(len-1)*channelInfo["wf_increment"],channelInfo["wf_increment"]];
	out=Transpose[{timeseries,scaled}]},
	out=scaled];
Close[file];
out	
]


(*TDMSProperties[{file,group*,channel*}] reads properties associated with the specified object within the TDMS file.
If no channel is provided, reads properties of the group, if no channel and group provided, reads properties of 
the file. This provides a convenient way to access information from the metadata that is not typically used 
by the user (i.e. Data)(without interacting directly with $TDMSInfo[file]).*)

TDMSProperties[path_List]:=Module[{file,pathString,properties},

file=path[[1]];

(*Check if the file provided can be opened and is a tdms file*)
If[tdmsOpen[file],
	Return["TDMSProperties: Unable to open file"]];

(*Check if the file specified has an associated tdms_index file which can be opened, use it to access metadata
going forward if it does*)
If[checkForIndex[file],
	If[!tdmsOpen[file<>"_index"],{
		debugPrint["TDMSProperties: index file found!"];
		metaFile=file<>"_index";},{
		debugPrint["TDMSProperties: index file invalid, using original file instead"];
		metaFile=file;}],{
	debugPrint["TDMSProperties: no index file found, using original file instead"];
	metaFile=file}];

(*Write TDMS path for requested address*)
pathString=tdmsPathWrite[path[[2;;]]];

(*Check if there is any saved metadata and search it for any relevant entries, if not then try to find it in the file*)
If[!AssociationQ[$TDMSInfo[file]],
	$TDMSInfo[file]=Association["Directory"-><||>]];
If[KeyExistsQ[$TDMSInfo[file],pathString],
	properties=$TDMSInfo[file][pathString],
	If[tdmsGrab[file,{pathString}],
		properties=$TDMSInfo[file][pathString],
		Return["TDMSProperties: Cannot find requested channel in file"]]];
Close[file];
properties
]


(*deBugPrint: is a purposefully undefined function. Normally it will do nothing.
When debugging, execute commands using:

"Block[{debugPrint=Print},*TDMSPublicFunctions*]"

and debugPrint will print the error information*)


(* ::Section:: *)
(*PRIVATE FUNCTIONS*)


(* ::Subsection:: *)
(*File open, and file verify functions*)


(* ::Text:: *)
(*general functions to open .tdms and .tdms_index files, verify they are not corrupted (or reached the end) and manage open streams*)


(*tdmsOpen: opens file and checks if it's a valid TDMS file, returns error status, sets stream position to 0*)


tdmsOpen[file_String]:=Module[{openStreams,error},

(*closes any streams with this name that are already open*)
openStreams=alreadyOpen[file];
If[openStreams>0,
	debugPrint["tdmsOpen: "<> ToString[openStreams]<> " stream(s) with this name was already open"]
	Do[Close[file],{ii,1,openStreams}]];

(*attempts to open file*)
If[OpenRead[file,BinaryFormat->True]===$Failed,{
	debugPrint["tdmsOpen: File not found"];
	error=True;},{
	(*verifies file is a tdms file*)
	If[tdmsCheck[file],{
		debugPrint["tdmsOpen: Not a TDMS file"];
		error=True;
		Close[file]},{

		error=False;
		(*resets position teo beginning of file (tdms Check advances stream by 4 bytes*)
		SetStreamPosition[file,0]}]}];
error]


(*alreadyOpen: checks if file is opened, returns the number of open streams with this name, 
mathematica will gladly open several streams with the same name, so this attempts to avoid issues caused by this*)


alreadyOpen[file_String]:=Module[{openStreams},
openStreams=Length[Streams[file]]]


(*tdmsCheck: checks for "TDSm" or "TDSh" tag which initializes a new segment in a TDMS file or TDMS_index file 
respectively, returns error status, advances the stream by 4 bytes (due to tdmsTag)*)


tdmsCheck[file_String]:=Module[{type,error},

(*Checks for a match with the tag appropriate for .tdms files, or .tdms_index files, depending on the filename*)
If[StringSplit[file,"_"][[-1]]==="index",
	type={"T","D","S","h"},
	type={"T","D","S","m"}];

If[tdmsTag[file]=!=type,{
	debugPrint["tdmsCheck: Stream not located at a beginning of a valid TDSM segment"];
	error=True;},
	error=False;];
error]


(*tdmsTag: reads a 4-character string for use with tdmsCheck, returns string as list of 4 characters,
advances the stream by 4 bytes*)


tdmsTag[file_String]:=BinaryRead[file,Table["Character8",{i,1,4}]]


(*checkForIndex: checks if a .tdms_index file exists, these are the same as a the tdms file but with all raw data
removed and with a different tag at the beginning of each segment*)


checkForIndex[file_String]:=FileNames[file<>"_index"]==={file<>"_index"};


(*segmentRead: reads the lead-in and metadata of a segment, then skips to the start of the next segment (Does not
read any data). Advances stream to the start of the next segment eithe by skipping the data (.tdms file)
or reading to the end of the metadata (.tdms_index file)*)


(* ::Subsection:: *)
(*Read functions*)


(* ::Text:: *)
(*Functions involved in reading organizational information and data properties of the file. Organized (as best as possible) as if they were going to be used to read the metadata continuously from start to finish, as they would be deployed using TDMSLoad. These functions are used selectively if we are trying to skip portions of the metadata to find specific information more quickly (the default for TDMSImport, TDMSPropertis and TDMSContents).*)


segmentRead[file_String,segmentLocation_Integer]:=Module[{segmentProperties,byteOrder,segmentLength,dataOffset},
debugPrint["in"];

(*Read table of contents (ToC), these properties will apply to all channels in the segment 
(ToC always little-endian, which is helpful since ToC specifiec endian-ness of the rest of the data)
advances the stream 4 bytes*)
segmentProperties=tdmsToC[file];

(*Abort if DAQmx encounteres*)
If[segmentProperties["DAQmx"],Abort[];]

(*set byte order to correctly read all numeric values in this segment*)
If[segmentProperties["BigEndian"],
byteOrder=1;,
byteOrder=-1;];

(*Skip version number, not needed, advances stream 4 bytes*)
Skip[file,"Byte",4];

(*Save segment length, 8 bytes*)
segmentLength=BinaryRead[file,"UnsignedInteger64",ByteOrdering->byteOrder];

(*Save data offset, 8 bytes, this is the length of the meta data between the current stream position and the
start of the data in this segment*)
dataOffset=BinaryRead[file,"UnsignedInteger64",ByteOrdering->byteOrder];

(*We are now at the end of the lead-in, which is 28 bytes from the start of the segment*)

(*add all lead in data to segment properties*)
AssociateTo[segmentProperties,{"SegmentLocation"->segmentLocation,"SegmentLength"->segmentLength,"DataOffset"->dataOffset}];	

(*read metadata (if any)*)
If[segmentProperties["MetaData"],
metaRead[file,segmentProperties,byteOrder]];
(*debugPrint[StreamPosition[file]];*)
(*Set position to next segment if we are using the actual file instead of the index file, must add 28 since segment length does not include the length of the lead in*)
If[StringSplit[file,"_"][[-1]]=!="index",
SetStreamPosition[file,segmentLocation+segmentLength+28]]
(*debugPrint[StreamPosition[file]];*)
(*if this is an index file then we should already be at the start of the next segment*)
segmentLength
]


(*tdmsToC: reads the ToC (table of contents) of a segment. This is a 4-byte, bitmap which flags what type(s)
of data the current segment contains and the order in which it is written*)


tdmsToC[file_String]:=Module[{toC,metaData,rawData,dAQmx,interleaved,bigEndian,newObjects},

toC=BinaryRead[file,"Integer32",ByteOrdering->-1];

metaData=BitAnd[toC,2]===2;
rawData=BitAnd[toC,8]===8;
dAQmx=BitAnd[toC,128]===128;
interleaved=BitAnd[toC,32]===32;
bigEndian=BitAnd[toC,64]===64;
newObjects=BitAnd[toC,4]===4;

<|"MetaData"-> metaData,"RawData"-> rawData,"DAQmx"-> dAQmx,"Interleaved"-> interleaved,"BigEndian"-> bigEndian,"NewObjects"-> newObjects|>]


(*metaRead: reads metadata of a segment. Reads each object in the metadata*)


metaRead[file_String,segmentProperties_Association,byteOrder_Integer]:=Module[{nIChannels,namedFile,nObjects,dataStart,interleavedChannels},
If[StringSplit[file,"_"][[-1]]==="index",
	namedFile=StringDrop[file,-6],
	namedFile=file];

nObjects=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];
dataStart=segmentProperties["SegmentLocation"]+segmentProperties["DataOffset"]+28;

interleavedChannels={};
Do[{dataStart,interleavedChannels}=objectRead[file,segmentProperties,byteOrder,dataStart,interleavedChannels],{i,1,nObjects}];

nIChannels=Length[interleavedChannels];
If[nIChannels>1,
	AssociateTo[$TDMSInfo[namedFile][interleavedChannels[[#]]],"IChannels"->interleavedChannels]&/@Range[nIChannels]]
]


(*objectRead: Determines whether each object is a file, group or channel. Associates the final properties of 
each channel to that channel's pathname (channel properties take priority over group properites, which take
priority over file properties). Adds each channel and group name to the directory*)


objectRead[file_String,segmentProperties_Association,byteOrder_Integer,dataStart_Integer,interleavedChannels_List]:=Module[{dataType,interleavedIndex,nextDataStart,namedFile,objectPath,directory,chanList,objectType,groupKey,dataProperties,nProperties,objectProperties,objectKey},

If[StringSplit[file,"_"][[-1]]==="index",
	namedFile=StringDrop[file,-6],
	namedFile=file];

(*read path of object*)
objectPath=tdmsPathRead[file,byteOrder];

(*determine from path whether object is file, group, or channel*)
If[objectPath==={},
	objectType="File",
	If[Length[objectPath]===1,
		objectType="Group",
		If[Length[objectPath]===2,
			objectType="Channel",{
			objectType="Unknown";
			debugPrint["objectRead: Unexpected path name, may contain extra '/' characters"]}]
]];

(*read data index for this object, if the segment contains any objects with data*)
If[segmentProperties["RawData"],
	dataProperties=dataIndex[file,byteOrder],
	Skip[file,"Byte",4]];

(*read number of properties associated with object*)
nProperties=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];

(*collect objectproperties*)
objectProperties=<||>;
Do[AssociateTo[objectProperties,propertyRead[file,byteOrder]],{i,1,nProperties}];

(*save properties for later if file, combine file and group properties if group, combine all properties if
channel. Associate with relevant pathname (add something to deal with if higher level directory is not defined)
also fix merging*)

directory=$TDMSInfo[namedFile]["Directory"];
objectKey=tdmsPathWrite[objectPath];
Which[objectType==="File",
	AssociateTo[$TDMSInfo[namedFile],objectKey->objectProperties],

	objectType==="Group",{
	If[!KeyExistsQ[directory,objectPath],
		AssociateTo[directory,objectPath[[1]]->{}]];
	AssociateTo[$TDMSInfo[namedFile],{objectKey->objectProperties,"Directory"->directory}];
},
	objectType==="Channel",{
	If[KeyExistsQ[directory,objectPath[[1]]],
		AppendTo[directory[objectPath[[1]]],objectPath[[2]]],
		AssociateTo[directory,objectPath[[1]]->{objectPath[[2]]}]];
		If[segmentProperties["Interleaved"],
			AppendTo[interleavedChannels,objectKey]];
	AssociateTo[$TDMSInfo[namedFile],{objectKey->Join[KeyTake[segmentProperties,"BigEndian"],KeyDrop[dataProperties,{"Data","DAQmx"}],objectProperties,<|"DataStart"->dataStart|>],"Directory"->directory}];
},	
	objectType==="Unknown",
	AssociateTo[$TDMSInfo[namedFile],objectKey->objectProperties]];

If[dataProperties["Data"]&&!segmentProperties["Interleaved"],{
	dataType=dataProperties["Type"];
	If[dataType=!="String",
		nextDataStart=dataStart+typeSize[dataType,dataProperties["Length"]],
		nextDataStart=dataStart+typeSize["String",dataProperties["Size"]]]},
	nextDataStart=dataStart];
{nextDataStart,interleavedChannels}]


(*tdmsPathRead: Reads a path string and splits it into group and channel. The base file should have an empty 
path*)

tdmsPathRead[file_String,byteOrder_Integer]:=Module[{pathLength,pathString,pathList},
pathLength=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];
pathString=StringJoin[BinaryRead[file,Table["Character8",{i,1,pathLength}]]];
pathList=StringTake[StringSplit[pathString,"/"],{2,-2}];
pathList]


(*dataIndex: reads the data index of an object*)

dataIndex[file_String,byteOrder_Integer]:=Module[{firstFour,hasData,dAQmx,dataType,nValues,dataSize},
firstFour=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];
If[firstFour===4294967295,{
	hasData=False;
	dAQmx=False;
	dataType="Void";
	nValues=0;
	dataSize=Null},
	If[firstFour===4713||firstFour===4969,{
		hasData=True;
		dAQmx=True;
		dataType="DAQmx";
		nValues=Null;
		dataSize=Null},{
		hasData=True;
		dAQmx=False;
		dataType=typeCatalog[BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder]];
		(*skip array size, not useful*)	
		Skip[file,"Byte",4];
		nValues=BinaryRead[file,"UnsignedInteger64",ByteOrdering->byteOrder];
		If[dataType==="String",
			dataSize=BinaryRead[file,"UnsignedInteger64",ByteOrdering->byteOrder];,
			dataSize=Null]}]];
If[hasData,
<|"Type" -> dataType, "Length" -> nValues, "Size" -> dataSize, "Data" -> hasData, "DAQmx" -> dAQmx|>,
<|"Data" -> hasData, "DAQmx" -> dAQmx|>]]


(*propertyRead: reads an object property, returns the property name associated with the property value*)

propertyRead[file_String,byteOrder_Integer]:=Module[{nameLength,propertyName,dataType,propertyValue},

propertyName=typeRead[file,"String",1,byteOrder];
dataType=typeCatalog[BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder]];
propertyValue=typeRead[file,dataType,1,byteOrder];
If[Head[propertyValue]===List,propertyValue=propertyValue[[1]]];
propertyName->propertyValue]


(*tdmsPathWrite: Writes the path as /group/channel for easy readibility (tdms paths usually have 
single quotes ' bracketing the group and channel names). To be used as keys in $TDMSInfo*)

tdmsPathWrite[pathList_List]:=Module[{pathLength,writtenPath},
pathLength=Length[pathList];
If[pathLength===0,
	writtenPath="/",
	If[pathLength===1,
		writtenPath="/"<>pathList[[1]];,
		If[pathLength===2,
			writtenPath="/"<>pathList[[1]]<>"/"<>pathList[[2]],{
			debugPrint["tdmsPathWrite: More than expected elements in path list"];			
			writtenPath="BADPATHWRITE"<>StringJoin[pathList];}]]];

writtenPath]


(* ::Subsection:: *)
(*Search and partially read functions*)


(* ::Text:: *)
(*Functions which skip over irrelevant parts of the metadata and/or selectively apply the read functions above to access the relevant information as quickly ass possible. Employed as the default if property information is unavailable for all functions except TDMSLoad. Skipping data is often only marginally faster than readding it (especially if we can only skip over small segments at a time) so for certain files (large defragmented files with the desired data towards the end of the file) it will be faster to just load all data.*)


(*takenames: scans through the file and picks out only the group and channel names so that the user can look up
only the data they want. Used with TDMSContents*)
takeNames[file_String]:=Module[{dataFile,namedFile,toC,byteOrder,segmentLength,dataOffset,directory,currentPosition,nObjects,nProperties,objectPath,objectType},

(*check if we are dealing with an index file or a full tdms file*)
If[StringSplit[file,"_"][[-1]]==="index",{
	dataFile=False;
	namedFile=StringDrop[file,-6]},{
	dataFile=True;
	namedFile=file}];

(*Check if there is metadata in this segment, if not then skip to next segment*)
	toC=tdmsToC[file];
	If[toC["BigEndian"],
		byteOrder=1,
		byteOrder=-1];
	Skip[file,"Byte",4]; (*skips over version code*)
	segmentLength=BinaryRead[file,"UnsignedInteger64",ByteOrdering->byteOrder];
	dataOffset=BinaryRead[file,"UnsignedInteger64",ByteOrdering->byteOrder];
	currentPosition=StreamPosition[file];
	If[!toC["MetaData"],{
		If[dataFile,
			SetStreamPosition[file,currentPosition+segmentLength],
			SetStreamPosition[file,currentPosition+dataOffset]]},{
		(*if there is metadata then extract all object names*)
		nObjects=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];
		directory=$TDMSInfo[namedFile]["Directory"];
		
		Do[ objectPath=tdmsPathRead[file,byteOrder];
			
			(*determine from path whether object is file, group, or channel*)
			If[objectPath==={},
				objectType="File",
				If[Length[objectPath]===1,
					objectType="Group",
					If[Length[objectPath]===2,
						objectType="Channel",{
						objectType="Unknown";
						debugPrint["objectRead: Unexpected path name, may contain extra '/' characters"]}]]];
			(*debugPrint[objectType];*)
			If[objectType==="Group",{
				If[!KeyExistsQ[directory,objectPath],
					AssociateTo[directory,objectPath[[1]]->{}]];
				AssociateTo[$TDMSInfo[namedFile],"Directory"->directory]}];
			If[objectType==="Channel",{
				If[KeyExistsQ[directory,objectPath[[1]]],
					AppendTo[directory[objectPath[[1]]],objectPath[[2]]],
					AssociateTo[directory,objectPath[[1]]->{objectPath[[2]]}]];
				AssociateTo[$TDMSInfo[namedFile],{"Directory"->directory}]}];
			(*debugPrint[StreamPosition[file]];*)		
			dataIndexSkip[file,byteOrder];
			(*debugPrint[StreamPosition[file]];*)
			nProperties=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];
			(*debugPrint[{nProperties,StreamPosition[file]}];*)
			Do[propertySkip[file,byteOrder],{i,1,nProperties}];
			(*debugPrint[StreamPosition[file]];*)
		,{i,1,nObjects}]}];
debugPrint[StreamPosition[file]];
If[dataFile,
	SetStreamPosition[file,currentPosition+segmentLength]];
]


(*dataIndexSkip: Reads as little of the data index as possible to skip over the rest of it. Since the data index
can only be 20 or 28 bytes long, and at least 4 bytes must be read it will be able to skip between 16 and 24 bytes*)
dataIndexSkip[file_String,byteOrder_Integer]:=Module[{firstFour,indexLength,currentPosition},
firstFour=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];
debugPrint[firstFour];
If[firstFour=!=4294967295,{
	currentPosition=StreamPosition[file];
	indexLength=firstFour;
	SetStreamPosition[file,currentPosition+indexLength-4]}];]


(*propertySkip: skips over an object property. Still has to read the property -ame length, the property-value data type,
and the property-value string length (if the property-value is a string)*)
propertySkip[file_String,byteOrder_Integer]:=Module[{stringLength,dataType},
stringLength=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];
skipIt[file,stringLength];
dataType=typeCatalog[BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder]];
If[dataType=!="String",
	skipIt[file,typeSize[dataType,1]],{
	stringLength=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];
	skipIt[file,stringLength]}];
]


(*skipIt: a modified version of Skip[file,"Byte",bytelength] that will skip by moving the low level pointer (using 
SetStreamPosition) if the streamposition is already known or if the length to be skipped justifies a call to
StreamPosition[] (judged to be ~10 Bytes on my machine)*)
skipIt[file_String,size_Integer,position_Integer :-1]:=Module[{pos},
If[position===-1,{
	(*debugPrint["skipIt: no position input"];*)
	If[size<10,
		Skip[file,"Byte",size],{
		pos=StreamPosition[file];
		SetStreamPosition[file,pos+size]}]},{
(*	debugPrint["skipIt: position is ",position];*)
	SetStreamPosition[file,position+size]}]
]


(*Searches the metadata for a list of pathnames and calls functions to save the properties of the objects at
these path adresses to $TDMSInfo[file]. Used with TDMSImport and TDMSProperties*)
tdmsGrab[file_String,keys_List]:=Module[{realPaths,findNObjects,nFound,totalFound,foundAll,end,dataPosition,newDataPosition},

findNObjects=Length[keys];
realPaths=writeRealPath[#]&/@keys;
(*key=tdmsPathWrite[StringTake[StringSplit[realPath,"/"],{2,-2}]];*)
totalFound=0;
(*foresee issues if the same channel group is defined multiple times in a file*)
dataPosition=0;
While[totalFound<findNObjects||end,
	{nFound,end,newDataPosition}=objectFind[file,keys,realPaths,dataPosition,findNObjects];
	dataPosition=newDataPosition;
	If[nFound>0,
		totalFound=totalFound+nFound]];
If[totalFound===findNObjects,
	foundAll=True,{
	foundAll=False;
	debugPrint["tdmsGrab: Found "<>totalFound<>"/"<>findNObjects<>" objects"]}];
foundAll]



(*reverses tdmsPathRead (writes the path with single quotes (') included so that the path we are looking for
can be compared to paths read from the file*)
writeRealPath[key_String]:=Module[{pathList,withQuotes,realPath},
pathList=StringSplit[key,"/"];
withQuotes=StringJoin["'",#,"'"]&/@pathList;
realPath="/"<>FileNameJoin[withQuotes,OperatingSystem->"Unix"]](*Using the Unix OS option makes the divider a forward slash*)


(*objectFind: Searches a segment for objects matching the given list of paths, associates their properties to $TDMSInfo
using the corresponding list of keys (both supplied by tdmsGrab)*) 
objectFind[file_String,keys_List,realPaths_List,inputDataFilePosition_Integer,findNObjects_Integer]:=Module[{dataType,nFound,hasData,outputDataFilePosition,dataPointer,endOfFile,namedFile,match,path,leadInPosition,matchedKey,dataFile,toC,objectProperties,nProperties,dataProperties,segmentLength,dataOffset,nObjects,byteOrder,pathLength,pathString},

nFound=0;
(*check if we are dealing with an index file or a full tdms file*)
If[StringSplit[file,"_"][[-1]]==="index",{
	dataFile=False;
	namedFile=StringDrop[file,-6]},{
	dataFile=True;
	namedFile=file}];
(*debugPrint[dataFile];*)
If[tdmsCheck[file],{
	debugPrint["objectFind: Could not find object in file, object does not exist or file is corrupted"];	
	endOfFile=True},{
	endOfFile=False;

(*Check if there is metadata in this segment, if not then skip to next segment*)
	toC=tdmsToC[file];
	If[toC["BigEndian"],
		byteOrder=1,
		byteOrder=-1];
	Skip[file,"Byte",4]; (*skips over version code*)
	segmentLength=BinaryRead[file,"UnsignedInteger64",ByteOrdering->byteOrder];
	dataOffset=BinaryRead[file,"UnsignedInteger64",ByteOrdering->byteOrder];
	dataPointer=inputDataFilePosition+28+dataOffset;
	leadInPosition=StreamPosition[file];
	If[toC["MetaData"],
(*if there is metadata then search it for an object with one of the input pathnames, get its properties and place stream at data position*)
		nObjects=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];
		Do[{match,path}=pathMatch[file,realPaths,byteOrder];
			dataProperties=dataIndex[file,byteOrder];
			hasData=dataProperties["Data"];
			If[match,{
				matchedKey=keys[[FirstPosition[realPaths,path]]][[1]];
				nFound++;
				(*debugPrint[dataProperties];*)
				nProperties=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];
				objectProperties=Association[Table[propertyRead[file,byteOrder],{i,1,nProperties}]];
				If[hasData,			
					AssociateTo[dataProperties,"DataStart"->dataPointer]];
				AssociateTo[$TDMSInfo[namedFile],matchedKey->Join[KeyTake[toC,"BigEndian"],KeyDrop[dataProperties,"DAQmx"],objectProperties]];
				},{
									
				(*debugPrint[nObjects," 1 ",StreamPosition[file]];*)
				nProperties=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];
				Do[propertySkip[file,byteOrder],{i,1,nProperties}]
				(*debugPrint[nObjects," 2 ",StreamPosition[file]];*)}];
			If[hasData,{
				dataType=dataProperties["Type"];
				If[dataType=!="String",
					dataPointer=dataPointer+typeSize[dataType,dataProperties["Length"]],
					dataPointer=dataPointer+typeSize["String",dataProperties["Size"]]]}];
			If[nFound===findNObjects,
				Break[];]
		,{i,1,nObjects}]];
	outputDataFilePosition=inputDataFilePosition+28+segmentLength;
	If[dataFile,
		SetStreamPosition[file,outputDataFilePosition],
		SetStreamPosition[file,leadInPosition+dataOffset]]}];

{nFound,endOfFile,outputDataFilePosition}]	


(*pathMatch: reads a path from file and checks if it matches any of the paths provided to it in a list*)


pathMatch[file_String,paths_List,byteOrder_Integer]:=Module[{pathLength,pathString,matchList,match,matchedPath},

pathLength=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];
pathString=StringJoin[BinaryRead[file,Table["Character8",{i,1,pathLength}]]];

matchList=(pathString===#)&/@paths;
match=AnyTrue[matchList,#&];
If[match,
	matchedPath=paths[[FirstPosition[matchList,True]]][[1]],
	matchedPath={}];
{match,matchedPath}]


(* ::Subsection:: *)
(*Convention lookup functions*)


(* ::Text:: *)
(*Functions for looking up conventions relating to data types in TDMS files*)


(*typeCatalog: interprets a TDMS type integer as the appropriate data type*)

typeCatalog[enumType_Integer]:=
<|0 -> "Void",
1 -> "Integer8",
2 -> "Integer16",
3 -> "Integer32",
4 -> "Integer64",
5 -> "UnsignedInteger8",
6 -> "UnsignedInteger16",
7 -> "UnsignedInteger32",
8 -> "UnsignedInteger64",
9 -> "Real32",
10 -> "Real64",
11 -> "Real128",
25 -> "UnitfulReal32",
26 -> "UnitfulReal64",
27 -> "UnitfulReal128",
32 -> "String",
33 -> "Boolean",
68 -> "TimeStamp",
79 -> "FixedPoint",
524300 -> "Complex64",
1048589 -> "Complex128",
4294967295 -> "DAQmx"|>[enumType]


(*determines the size of data in bytes*)

typeSize[type_String,length_Integer]:=Module[{nBytes},
Which[
AnyTrue[{"Void","Integer8","UnsignedInteger8","Boolean"},#===type&],
nBytes=length,

AnyTrue[{"Integer16","UnsignedInteger16"},#===type&],
nBytes=length*2,

AnyTrue[{"Integer32","UnsignedInteger32","Real32","UnitfulReal32","FixedPoint"},#===type&],
nBytes=length*4,

AnyTrue[{"Integer64","UnsignedInteger64","Real64","Complex64","UnitfulReal64"},#===type&],
nBytes=length*8,

AnyTrue[{"Real128","Complex128","UnitfulReal128"},#===type&],
nBytes=length*16,

type==="String",{
debugPrint["typeSize: Warning, for datatype: 'String', 'length' parameter must correspond to the total size of string"];
nBytes=length},

type==="TimeStamp",
nBytes=length*16,

type==="DAQmx",
debugPrint["typeSize: Cannot handle DAQmx right now"];

nBytes]]


(*typeRead: reads n pieces of information of a known data type, can read strings from the data index where
the length of the string is always 1 (word). But cant read string data since they are organized in very different
 ways*)

typeRead[file_String,type_String,length_Integer,byteOrder_Integer]:=Module[{out,stringLength},
Which[
type==="Void",
out=BinaryRead[file,Table["Integer8",{i,1,length}],ByteOrdering->byteOrder],

AnyTrue[{"Integer8","Integer16","Integer32","Integer64","UnsignedInteger8","UnsignedInteger16","UnsignedInteger32","UnsignedInteger64","Real32","Real64","Real128","Complex64","Complex128"},#===type&],
out=BinaryRead[file,Table[type,{i,1,length}],ByteOrdering->byteOrder],

AnyTrue[{"UnitfulReal32","UnitfulReal64","UnitfulReal128"},#===type&],
out=BinaryRead[file,Table[StringDrop[type,7],{i,1,length}],ByteOrdering->byteOrder],

type==="String",
If[length==1,{
	stringLength=BinaryRead[file,"UnsignedInteger32",ByteOrdering->byteOrder];
	out=StringJoin[BinaryRead[file,Table["Character8",{i,1,stringLength}]]]},
	out=stringDataRead[file,byteOrder]],

type==="Boolean",
	out=(#!=0)&/@BinaryRead[file,Table["Integer8",{i,1,length}]],

type==="TimeStamp",{
	out=Table[FromUnixTime[BinaryRead[file,"UnsignedInteger64",ByteOrdering->byteOrder]*2^-64+BinaryRead[file,"Integer64",ByteOrdering->byteOrder]-2082830400],{i,1,length}]},

type==="FixedPoint",{
	debugPrint["typeRead: TypeRead reads Fixed Point values as 32 bit integers for now"];
	out=BinaryRead[file,Table["Integer32",{i,1,length}],ByteOrdering->byteOrder]},

type==="DAQmx",{
	debugPrint["typeRead: Cannot handle DAQmx right now"]
	out=Null}];
out]


(* ::Subsection:: *)
(*Obsolete functions*)


(* ::Text:: *)
(*Functions that are not currently used in any public functions but are held on to incase they are useful later*)


(*tdmsIndex (OBSOLETE): generates a list of starting points for each segment. Not used anywhere, written when I 
first started to verify I was understanding things correctly, still possibly helpful as a reference since I know 
this works*)

tdmsIndex[file_String]:=Module[{segmentIndex,segmentLength,segmentPosition,err},
If[tdmsOpen[file],{
	debugPrint["TDMSIndex: Cannot open file"];
	err=True;
	segmentIndex=Null;},

	SetStreamPosition[file,12]; (*12 is location of segment size in lead-in*)
	segmentIndex=Reap[While[
		(segmentLength=BinaryRead[file,"Integer64"])=!=EndOfFile,
			segmentPosition=StreamPosition[file]-20; (*20 is distance from end of segment size from beginning of segment*)
			Sow[segmentPosition];
			SetStreamPosition[file,segmentPosition+segmentLength+28]; (*28 is size of lead-in*)
		If[tdmsCheck[file],Break[]]; 
			SetStreamPosition[file,segmentPosition+segmentLength+28+12]]][[2,1]](*go to next segment size*)
	Close[file]]
{segmentIndex,err}];



End[];


EndPackage[];
