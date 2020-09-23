(* ::Package:: *)

call[x_,p_,c_]:=If[x<p,c,x-p+c];
put[x_,p_,c_]:=If[x>p,c,p-x+c];


calls[s_,e_,step_,p_,c_]:=call[#,p,c]&/@Range[s,e,step];
puts[s_,e_,step_,p_,c_]:=put[#,p,c]&/@Range[s,e,step];


findSimplePrice[s_,e_,step_,p_]:={Flatten[NSolve[Total[calls[s,e,step,p,x]]==0]][[1]][[2]],Flatten[NSolve[Total[puts[s,e,step,p,x]]==0]][[1]][[2]]};
