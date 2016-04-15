:- module(ciao_yices_2,[
	yices_version/1,
	yices_init/0,
	yices_exit/0,
	yices_reset/0,
	yices_context/1,
	yices_free_context/1,
	yices_parse_term/2,
	yices_assert_formula/3,
	yices_check/2,
	yices_context_status/2,
	yices_status/2,
	yices_declare_real/1,
	yices_declare_int/1,
	yices_declare_bool/1,
	yices_declare_function1/3,
	yices_term_is_bool/2,
	yices_term_to_string/5,
	yices_error_string/1,
	yices_get_model/3,
	yices_get_int32_value/4,
	yices_get_term_by_name/2,
	yices_get_value_as_term/3,
	yices_formula_true_in_model/3
   ], [assertions,
       basicmodes,
       regtypes,
       foreign_interface]).

% Ciao foreign language interface for Yices 2.3.1
% See http://yices.csl.sri.com/
% Pointer to the Yices manual: http://yices.csl.sri.com/papers/manual.pdf

:- foreign_inline("
#include <yices.h>
").

:- true pred yices_version(go(V)) :: 
	string + (foreign(ciao_yices_version), returns(V), do_not_free(V)).
:- foreign_inline(ciao_yices_version/1, "
const char *ciao_yices_version() {
  return yices_version;
}
").
 		
:- true pred yices_init + foreign.

:- true pred yices_exit + foreign.

:- true pred yices_reset + foreign.
 		
:- true pred yices_new_context(in(Config),go(Ctx)) :: 
 		address * address + (foreign, returns(Ctx)).
 		
:- true pred yices_free_context(in(Ctx)) :: 
 		address + foreign.
 		
:- true pred yices_set_term_name(in(T),in(Name),go(TermIndex)) :: 
 		c_int32 * string * c_int32 + (foreign, returns(TermIndex)).
 		              
:- true pred yices_parse_term(in(S),go(TermIndex)) :: 
 		string * c_int32 + (foreign, returns(TermIndex)).
 		
:- true pred yices_assert_formula(in(Ctx),in(T), go(Status)) :: 
 		address * c_int32 * c_int32 + (foreign, returns(Status)).
 		
:- true pred yices_check_context(in(Ctx),in(Params),go(Sat)) :: 
 		address * address * c_int32 + (foreign, returns(Sat)).
 		
:- true pred yices_context_status(in(Ctx),go(Status)) :: 
 		address * c_int32 + (foreign, returns(Status)).
 		
:- true pred yices_new_uninterpreted_term(in(Tau),go(V)) ::
		c_int32 * c_int32 + (foreign, returns(V)).
		
:- true pred yices_new_variable(in(Tau),go(V)) ::
		c_int32 * c_int32 + (foreign, returns(V)).
		
:- true pred yices_real_type(go(Real)) ::
		c_int32 + (foreign, returns(Real)).
	
:- true pred yices_int_type(go(Int)) ::
		c_int32 + (foreign, returns(Int)).
		
:- true pred yices_bool_type(go(Bool)) ::
		c_int32 + (foreign, returns(Bool)).	

% Creates the unary function type (-> Tau1 Range)
:- true pred yices_function_type1(in(Tau1), in(Range), go(Fun)) ::
		c_int32 * c_int32 * c_int32 + (foreign, returns(Fun)).

:- true pred yices_term_is_bool(in(T),go(B)) ::
		c_int32 * c_int32 + (foreign, returns(B)).	
		
:- true pred yices_term_to_string(in(T),in(Width), in(Height), in(Offset), go(TF)) ::
	c_int32 * c_uint32 * c_uint32 * c_uint32 * string + (foreign, returns(TF))
   # "Converts a term to a string".

:- true pred yices_error_string(go(E)) ::
		string + (foreign, returns(E)).		
			
:- true pred yices_get_model(in(Ctx),in(KeepSubst),go(Model)) ::
		address * c_int32 * address + (foreign, returns(Model)).
		
:- true pred yices_get_int32_value(in(Model),in(T),go(Val),go(Status)) ::
		address * c_int32 * c_int32 * c_int32 + (foreign, returns(Status)).	
		
:- true pred yices_get_term_by_name(in(Name),go(Term)) ::
		string * c_int32 + (foreign, returns(Term)).
		
:- true pred yices_get_value_as_term(in(Model),in(T),go(Term)) ::
		address * c_int32 * c_int32 + (foreign, returns(Term)).	
		
:- true pred yices_formula_true_in_model(in(Model),in(F),go(TF)) ::
		address * c_int32 * c_int32 + (foreign, returns(TF)).	

:- include(.(ciao_yices_config_auto)).

:- use_foreign_library(yices).

yices_context(Ctx) :-
	null(Null),
	yices_new_context(Null,Ctx).
	
yices_check(Ctx,StatusName) :-
	null(Null),
	yices_check_context(Ctx,Null,Status),
	status(Status,StatusName).
	
yices_declare_real(X) :-
	yices_declare(real, X).

yices_declare_int(X) :-
	yices_declare(int, X).

yices_declare_bool(X) :-
	yices_declare(bool, X).
 
yices_declare_function1(Tau1, Range, X) :-
	yices_type(Tau1, Tau1T),
	yices_type(Range, RangeT),
	yices_declare(function1(Tau1T, RangeT), X).

yices_declare(Tau, X) :-
	yices_type(Tau, TauT),
	yices_new_uninterpreted_term(TauT,V),
	yices_set_term_name(V,X,Status),
	( Status == 0 -> true
	; throw(error(failed_to_declare(Tau, X), yices_declare/2))
	).

yices_type(bool, Tau) :- yices_bool_type(Tau).
yices_type(int, Tau) :- yices_int_type(Tau).
yices_type(real, Tau) :- yices_real_type(Tau).
yices_type(function1(Tau1, Range), Tau) :- yices_function_type1(Tau1, Range, Tau).

yices_status(Ctx,StatusName) :-
	yices_context_status(Ctx,Status),
	status(Status,StatusName).
	
status(0,idle).
status(1,searching).
status(2,unknown).
status(3,satisfiable).
status(4,unsatisfiable).
status(5,interrupted).
status(6,error).
