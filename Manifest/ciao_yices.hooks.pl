:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Ciao-Yices").

'$builder_hook'(desc_name('Ciao-Yices')).

% ============================================================================

:- use_module(library(process), [process_call/3]).
:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(ciaobld(ciaoc_aux), [build_libs/2]).

'$builder_hook'(prebuild_nodocs) :-
	aux_call(['fetch']),
	aux_call(['gen_conf']).

'$builder_hook'(build_libraries) :-
	build_libs(ciao_yices, 'src').

'$builder_hook'(install) :- bundleitem_do(only_global_ins(~ciao_yices_desc), ciao_yices, install).

'$builder_hook'(uninstall) :- bundleitem_do(only_global_ins(~ciao_yices_desc), ciao_yices, uninstall).

ciao_yices_desc := [
  lib(ciao_yices, 'src')
].

% ---------------------------------------------------------------------------
% Run tests

:- use_module(library(system), [working_directory/2]).
:- use_module(ciaobld(ciaoc_aux), [invoke_ciaosh_batch/1]).

% TODO: use unittests
'$builder_hook'(runtests) :- !,
	working_directory(ThisDir, ~fsR(bundle_src(ciao_yices))),
	invoke_ciaosh_batch([
	  use_module(test, [test/1]),
	  ( test(_), fail ; true )
	]),
	working_directory(_, ThisDir).

% ---------------------------------------------------------------------------
% (call external scripts and makefiles)

% (will not work in Windows)
aux_sh := ~fsR(bundle_src(ciao_yices)/'Manifest'/'gen-conf.sh').

aux_call(Args) :- 
	process_call(~aux_sh, Args, []).


