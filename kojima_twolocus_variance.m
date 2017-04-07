(* ::Package:: *)

(* ::Title:: *)
(*Kojima's method applied to a weird two locus genetic model*)


(* ::Subsection:: *)
(*Define a weird genetic model*)


(* ::Text:: *)
(*Okay, so we make a model where there are two loci and thus nine genotypes. we can make a model that looks somewhat normal, but actually turns out to be weird.*)


(* ::Input:: *)
(*G ={{2a, 1.5*a,0},{ a,0.5,-a},{0,-a*1.5,-2a}}*)


(* ::Input:: *)
(*G//MatrixForm*)
(**)


(* ::Text:: *)
(*We need to make these vectors of possible marginal genotype frequencies in order to flesh the thing out. Mathematicas TensorProduct[] function woudl allow us to generalize this to arbitrary numbers of loci. Perhaps there is an automatic way to do this.*)


(* ::Input:: *)
(*PA = {p1^2, 2*p1*(1-p1),(1-p1)^2}*)
(*PB = {p2^2, 2*p2*(1-p2),(1-p2)^2}*)


(* ::Text:: *)
(**)


(* ::Output:: *)
(*{p1^2,2 (1-p1) p1,(1-p1)^2}*)


(* ::Input:: *)
(*mat = TensorProduct[PA,PB]*G*)


(* ::Text:: *)
(*This is now a matrix of the contributions of each genotypic class to the population mean. To get the populaiton mean, we have to this sum up. We can do this by using Total[] or by crossproduct with vector of ones.*)


(* ::Input:: *)
(*mat //MatrixForm*)


(* ::Input:: *)
(*vec = ConstantArray[1,3]*)


(* ::Input:: *)
(*vec.mat.vec*)


(* ::Input:: *)
(* u[a_,p1_,p2_] = Total[G* TensorProduct[PA,PB],{1,2}]*)


(* ::Text:: *)
(**)


(* ::Subsection:: *)
(*Derive some variance components using Kojimas notation*)


(* ::Subsection:: *)
(*Additive variance*)


(* ::Text:: *)
(*We have two loci, so the additive variance will involve  taking the first derivative of the populaiton mean at both loci and multiplying respectively by the heterozgyosity at that loci.*)


(* ::Input:: *)
(*VA[a_,p1_,p2_] = 0.5* p1*(1-p1)*D[u[a,p1,p2],p1]^2 +  0.5*p2*(1-p2)*D[u[a,p1,p2],p2]^2 *)


(* ::Text:: *)
(**)


(* ::Input:: *)
(**)
(*VA[2,0.4,0.4]*)


(* ::Input:: *)
(*Plot3D[VA[1,p1,p2],{p1,0,1},{p2,0,1},AxesLabel->{Subscript[p, 1],Subscript[p, 2],VA},BaseStyle->{FontSize-> 12}]*)


(* ::Input:: *)
(*ContourPlot[VA[1,p1,p2],{p1,0,1},{p2,0,1},AxesLabel->{Subscript[p, 1],Subscript[p, 2],VA},BaseStyle->{FontSize-> 12}]*)


(* ::Subsection:: *)
(*Dominance variance*)


(* ::Text:: *)
(*Wow, that additive variance looks strange huh! But, if you change the original model to a more basic form, you will see that it is correct. Now we do the dominance.*)


(* ::Input:: *)
(**)
(*VD[a_,p1_,p2_] = 0.25* ((p1*(1-p1))^2)*D[u[a,p1,p2],{p1,2}]^2 +  0.25* ((p2*(1-p2))^2)*D[u[a,p1,p2],{p2,2}]^2 *)


(* ::Subsection:: *)
(**)


(* ::Input:: *)
(*PercVA[a_,p1_,p2_]:= VA[a,p1,p2]/(VA[a,p1,p2] + VD[a,p1,p2])*)
(*VAVD[a_,p1_,p2_]:= VA[a,p1,p2] + VD[a,p1,p2]*)


(* ::Input:: *)
(*Plot3D[VD[1,p1,p2],{p1,0,1},{p2,0,1},AxesLabel->{Subscript[p, 1],Subscript[p, 2],VD},BaseStyle->{FontSize-> 12}]*)
(**)


(* ::Input:: *)
(*ContourPlot[VD[1,p1,p2],{p1,0,1},{p2,0,1},AxesLabel->{Subscript[p, 1],Subscript[p, 2],VAA},BaseStyle->{FontSize-> 12}]*)


(* ::Subsection:: *)
(*Additive by additive epistatic variance*)


(* ::Input:: *)
(*VAA[a_,p1_,p2_] =  0.25*(p1*(1-p1))*(p2*(1-p2))*D[u[a,p1,p2],p1,p2]^2 *)


(* ::Input:: *)
(*Plot3D[VAA[1,p1,p2],{p1,0,1},{p2,0,1},AxesLabel->{Subscript[p, 1],Subscript[p, 2],VAA},BaseStyle->{FontSize-> 12}]*)


(* ::Input:: *)
(*ContourPlot[VAA[1,p1,p2],{p1,0,1},{p2,0,1},AxesLabel->{Subscript[p, 1],Subscript[p, 2],VAA},BaseStyle->{FontSize-> 12}]*)


(* ::Subsection:: *)
(*Narrow sense heritability, aka the percent of total variance that is due to additive variance*)


(* ::Input:: *)
(*PercVA[a_,p1_,p2_] := VA[a,p1,p2]/(VA[a,p1,p2] +VD[a,p1,p2] + VAA[a,p1,p2])*)


(* ::Input:: *)
(*PercVA[a,p1,p2]*)


(* ::Input:: *)
(*ContourPlot[PercVA[.1,p1,p2],{p1,0,1},{p2,0,1},FrameLabel->{Subscript[p, 1],Subscript[p, 2]},BaseStyle->{FontSize-> 12},ContourLabels->All]*)
