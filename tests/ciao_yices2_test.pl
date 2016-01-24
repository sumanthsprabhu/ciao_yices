:- module(ciao_yices2_test,_).

:- use_module(ciao_yices(ciao_yices_2)).
:- use_module(library(strings)).

main :-
	test1,
	test2.

test1 :-
	yices_init,
	yices_context(Ctx),
	yices_declare_real("x"),
	yices_declare_real("y"),
	yices_parse_term("(= (+ x y) 0)",T1),
	yices_parse_term("(>= y 0)",T2),
	yices_parse_term("(>= x 0)",T3),
	yices_status(Ctx,S0),
	write(S0),nl,
	yices_assert_formula(Ctx,T1,_Status1),
	yices_assert_formula(Ctx,T2,_Status2),
	yices_assert_formula(Ctx,T3,_Status3),
	yices_check(Ctx,StatusName),
	write(StatusName),nl,
	yices_get_term_by_name("x",X),
	yices_get_term_by_name("y",Y),
	(StatusName == satisfiable -> 
		yices_get_model(Ctx,0,Model),
		yices_get_value_as_term(Model,X,ValX),
		yices_get_value_as_term(Model,Y,ValY),
		write('X = '),write(ValX),nl,
		write('Y = '),write(ValY),nl,
		yices_parse_term("(= y 0)",T4),
		yices_parse_term("(= x 1)",T5),
		yices_formula_true_in_model(Model,T4,TF1),
		yices_formula_true_in_model(Model,T5,TF2),
		write('TF1 = '),write(TF1),nl,
		write('TF2 = '),write(TF2),nl
		;
		write('No model')),
	yices_free_context(Ctx),
	yices_exit.
  
test2 :-
	yices_init,
	yices_context(Ctx),
	yices_declare_real("x"),
	yices_declare_real("y"),
	yices_parse_term("(= (+ x y) 0)",T1),
	yices_term_to_string(T1, 1000000, 1, 0, T1Str),
	format("T1 = ~s~n", T1Str),
	yices_free_context(Ctx),
	yices_exit.

reportError(S) :-
	S<0 -> reportErrorState; true.
	
reportErrorState :-
	yices_error_string(E),
	write_string(E),
	nl.
	
