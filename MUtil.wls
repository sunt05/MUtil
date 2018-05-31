#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Subsubsection:: *)
(*compact form of the function: subdivTd*)


Clear[subdivTd]
(*sub-divide TemporalData into small windwows within large windows;
e.g., monthly climatology of diurnal cycles*)
subdivTd[td_TemporalData,wSmall_,wLarge_,fun_:Identity]:=
Module[{subSmall,subLarge,timeSmall},
(*first, aggregate into small windows*)
subSmall=TimeSeriesAggregate[td,wSmall,Identity];
timeSmall=td["Dates"][[{1,subSmall["ValueDimensions"]/.{h_,l_}:>h}]];
(*first, aggregate into large windows; 
N.B., Hold for wrapping data of different dimensions between different large windows*)
subLarge=TimeSeriesAggregate[subSmall,wLarge,Hold];
(*thread `fun` over values of small windows within a large window*)
TimeSeriesMap[TimeSeriesThread[fun,TemporalData[#/.Hold->Identity,timeSmall]]&,subLarge]
]
