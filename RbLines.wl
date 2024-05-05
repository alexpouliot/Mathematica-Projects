(* ::Package:: *)

(* ::Section:: *)
(*BEGIN PACKAGE*)


BeginPackage["RbLines`"];
Lorentz::usage = "normalized Lorentzian function, variables (width, offset, independant variable)";
Gauss::usage = "normalized Gaussian function, variables (independant variable, offset, sigma)";
GaussFWHM::usage = "normalized Gaussian function with FWHM as width parameter variables (independant variable, offset, FWHM)";
Voigt::usage = "approximation to Voigt function with variables (\[Delta],\[Sigma],x), (Lorentzian width, gaussian width, center point) using the Quine paper";
RbLine::usage = "function for single D2 Rb line, input (isotope, ground F, excited F',pressure (any unit),temperature (K),broadening (Hz/pressure unit), shift (Hz/pressure unit), independant variable (Hz)";
NatRbLines::usage = " function for all D2 Rb lines, input (no. density of Rb (any isotope),length of cell (m),pressure (any unit), temperature (K),broadening (Hz/pressure unit), shift (Hz/pressure unit), independant variable (Hz)";
Rb87Lines::usage = "Only the 87 lines";
NatRbLinesFree::usage = "Function for fitting spectrum to natural Rb sample but with free shift and width parameters"
(*NatRbLinesCompiled::usage = "Compiled version";*)


levelLookup::usage = "get amplitude of Rb line";
\[Nu]::usage = "Transition locations for D2";
Hs::usage = "Excited states accessible by electric dipole transistions from a certain ground state";
Gs::usage = "Possible ground states for a certain isotope of Rb";
\[CapitalGamma]N::usage = "Natural linewidth for each Rb isotope";


Begin["Private`"];


(* ::Section:: *)
(*PUBLIC FUNCTIONS*)


Lorentz[\[CapitalGamma]_,x0_,x_]:=\[CapitalGamma]/(2 \[Pi]) 1/((\[CapitalGamma]/2)^2+(x-x0)^2);
Gauss[t_,t0_,\[Sigma]_]:=1/(\[Sigma] Sqrt[2\[Pi]]) Exp[-(1/2) ((t-t0)/\[Sigma])^2];
GaussFWHM[t_,t0_,w_]:=(2Sqrt[Log[2]])/(w Sqrt[\[Pi]]) Exp[(-(t-t0)^24 Log[2])/w^2];
Voigt=With[{n=24,\[Tau]=12},With[{d=N[Range[n] \[Pi]/\[Tau]],b=N[Exp[-(Range[n] \[Pi]/\[Tau])^2]],s=N[PadRight[{},n,{-1,1}]],sq=N[Sqrt[2]],sp=N[Sqrt[2 \[Pi]]]},Compile[{{\[Delta],_Real},{\[Sigma],_Real},{x,_Real}},Module[{z=(x+I \[Delta])/(\[Sigma] sq),e},e=Exp[I \[Tau] z];
	Re[(I (1-e)/(\[Tau] z)+(2 I z/\[Tau]) b . ((e s-1)/((d+z) (d-z))))]/(\[Sigma] sp)],RuntimeAttributes->{Listable}]]];
RbLine[A_,G_,H_,P_,T_,Br_,Sh_,\[Nu]p_]:=levelLookup[A,G,H]Voigt[(\[CapitalGamma]N[A]+Br P)/2,Sqrt[(kB T)/mRb[A]] \[Nu][A,G,H]/c,\[Nu][85,3,4]+\[Nu]p-\[Nu][A,G,H]-Sh P];
RbLineFree[A_,G_,H_,T_,\[CapitalGamma]P_,\[Delta]P_,\[Nu]p_]:=levelLookup[A,G,H]Voigt[(\[CapitalGamma]N[A]+\[CapitalGamma]P)/2,Sqrt[(kB T)/mRb[A]] \[Nu][A,G,H]/c,\[Nu][85,3,4]+\[Nu]p-\[Nu][A,G,H]-\[Delta]P];
NatRbLines[nTot_,len_,P_,T_,Br_,Sh_,\[Nu]p_]:=nTot len Sum[RbLine[A,G,H,P,T,Br,Sh,\[Nu]p],{A,{85,87}},{G,Gs[A]},{H,Hs[G]}];
Rb87Lines[nTot_,len_,P_,T_,Br_,Sh_,\[Nu]p_]:=nTot/0.278 len Sum[RbLine[87,G,H,P,T,Br,Sh,\[Nu]p],{G,Gs[87]},{H,Hs[G]}];
NatRbLinesFree[nTot_,len_,T_,\[CapitalGamma]P_,\[Delta]P_,\[Nu]p_]:=nTot len Sum[RbLineFree[A,G,H,T,\[CapitalGamma]P,\[Delta]P,\[Nu]p],{A,{85,87}},{G,Gs[A]},{H,Hs[G]}];
(*NatRbLinesCompiled=Compile[{{nTot,_Real},{len,_Real},{P,_Real},{T,_Real},{Br,_Real},{Sh,_Real},{\[Nu]p,_Real}},nTot len Sum[RbLine[A,G,H,P,T,Br,Sh,\[Nu]p],{A,{85,87}},{G,Gs[A]},{H,Hs[G]}]];*)


(* ::Section:: *)
(*PRIVATE FUNCTIONS*)


(* ::Subsection:: *)
(*Definitions*)


(* ::Subsubsection:: *)
(*Physical constants*)


a0=5.291772 10^-11; (*Bohr radius in m*)
e=1.6021766 10^-19;(*electron charge in C*)
\[HBar]=1.054572 10^-34;(*reduced planck constant in J s*)
c=299792458; (*speed of light in m/s*)
kB=1.380649 10^-23;(*Boltzmann constant in J/K*)
m0=1.66054 10^-27;(*average nucleon mass in kg*)
\[Epsilon]0=8.85419 10^-12;(*vacuum permittivity m^-3kg^-1s^4A^2*)


(* ::Subsubsection:: *)
(*Angular Momentum definitions*)


Hs[G_]:=Range[G-1,G+1]; (*Excited states accesible by electric dipole transistions from ground state G*)
g[j_]:=2j +1; (*no. of substates in state with max ang. momentum j*)


(*Total oscillator strength for F=G->F'=H D2 (L=0 to L=1/2) transition, with nuclear ang. mom. II[A] in units of |\[LeftAngleBracket]J=1/2||e r||J'=3/2\[RightAngleBracket]|^2*)
Off[ClebschGordan::phy];
S[A_,G_,H_]:=Sum[((-1)^(H+1-mG) Sqrt[(2G +1)(2H+1)]ThreeJSymbol[{G,-mG},{1,q},{H,mH}]SixJSymbol[{G,1,H},{3/2,II[A],1/2}])^2,{q,-1,1,2},{mG,-G,G},{mH,-H,H}]; 


(* ::Subsubsection:: *)
(*Constants related to Rb*)


(*Transition frequencies for Rb isotope number, F, F'*)
\[Nu][A_,G_,H_] :=Which[A==85,
		Which[G==2,
			Which[H==1,384.230406373 10^12+1.7708439228 10^9-113.208 10^6,
				H==2, 384.230406373 10^12+1.7708439228 10^9-83.835 10^6,
				H==3,384.230406373 10^12+1.7708439228 10^9-20.435 10^6],
			G==3,
			Which[H==2,384.230406373 10^12-1.2648885163 10^9-83.835 10^6,
				H==3,384.230406373 10^12-1.2648885163 10^9-20.435 10^6,
				H==4,384.230406373 10^12+100.205 10^6-1.2648885163 10^9]],
		A==87,
		Which[G==1,
			Which[H==0,384.2304844685 10^12+4.27167663181519 10^9-302.0738 10^6,
				H==1, 384.2304844685 10^12+4.27167663181519 10^9-229.8518 10^6,
				H==2,384.2304844685 10^12+4.27167663181519 10^9-72.9113 10^6],
			G==2,
			Which[H==1,384.2304844685 10^12-2.56300597908911 10^9-229.8518 10^6,
				H==2,384.2304844685 10^12-2.56300597908911 10^9-72.9113 10^6,
				H==3,384.2304844685 10^12-2.56300597908911 10^9+193.7408 10^6]]]

II[A_]:=Which[A==85,5/2,A==87,3/2]; (*Nuclear spin of each nat. isotope*)
(*Steck, natural linewidth in Hz for Rb (multiply by 2\[Pi] for decay rate)*)
\[CapitalGamma]N[A_]:=Which[A==85,6.0666 10^6,
A==87,6.065 10^6]; 
mRb[A_]:= m0 A; (*mass of isotope A in kg*)
Gs[A_]:=Which[A==85,{2,3},A==87,{1,2}]; (*ground states for nat. isotopes of Rb*)
Ab[A_,G_]=Which[A==85,g[G]/Sum[g[F],{F,Gs[85]}] 0.722,A==87,g[G]/Sum[g[F],{F,Gs[87]}] 0.278];(*Natural abundance with each ground state sublevel equally populated*)
inPJ[A_]:=Which[A==85,4.2275362 a0 e,A==87,4.2275262 a0 e]; (*inner product \[LeftAngleBracket]J=1/2||e r||J'=3/2\[RightAngleBracket] in all directions*)


(* ::Subsection:: *)
(*Calculations*)


f[A_,G_,H_]:=(2 mRb[A] \[Nu][A,G,H])/(3 \[HBar] g[G]) S[A,G,H]inPJ[A]^2;(*oscillator strength*)
exConst[A_]:= \[Pi]/(2 \[Epsilon]0 mRb[A] c);(*coefficient in absorbtion exponential *)
(*Make lookup table for product of entire exponential coefficient to make functions more efficient*)
coTab=Table[exConst[A] Ab[A,G]f[A,G,H],{A,{85,87}},{G,Gs[A]},{H,Hs[G]}];
(*location in table of isotope, ground, excited triplet*)
levelPart[A_,G_,H_]:=Module[{isotopeList,gList,hList,firstD,secondD,thirdD},
	isotopeList={85,87};
	gList={{2,3},{1,2}};
	hList=Range[G-1,G+1];
	firstD=Position[isotopeList,_?(#==A&)][[1,1]];
	secondD=Position[gList[[firstD]],_?(#==G&)][[1,1]];
	thirdD=Position[hList,_?(#==H&)][[1,1]];
{firstD,secondD,thirdD}
];

(*lookup *)
levelLookup[A_,G_,H_]:=ReleaseHold[Hold[coTab[[i,j,k]]]/.AssociationThread[{i,j,k},levelPart[A,G,H]]];


(* ::Section::Closed:: *)
(*END PACKAGE*)


End[];
EndPackage[];
