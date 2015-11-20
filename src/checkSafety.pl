:- module(checkSafety,_).


safe(PFile, K) :-
	open(PFile,read,S),
	read(S,C),
	(checkForFalse(S,C, K) ->
		close(S); 
		close(S),
		fail).

	
checkForFalse(_,end_of_file, _) :-
	!.
checkForFalse(S,(H:-_), K) :-
    name(K, NK),
    append([91|NK], [93], RB), % 91=[ and 93=]
    name(false, FS),
    append(FS, RB, FPredN),
    name(FPred, FPredN),
	H \== FPred,
	read(S,C1),
	checkForFalse(S,C1, K).
	
main([F, K]) :-
	%open('pec_benchmarks.txt',append,S),
    S = user_output,
	write(S,F), 
	(safe(F, K) ->
		write(S,': PROGRAM IS SAFE'),nl(S), halt(0);
		write(S,': PROGRAM MIGHT NOT BE SAFE'),nl(S), halt(1)),
	close(S).
	

	
