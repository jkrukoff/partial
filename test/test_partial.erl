%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/partial.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_partial).

% This triggers the option error handling and prevents the rest of the
% test from running.
% -compile({partial_allow_local, invalid}).

-compile([{parse_transform, partial},
          partial_allow_local]).

-export([dummy/0,
         dummy/1,
         dummy/3]).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

dummy() ->
    true.

dummy(Arg1) ->
    Arg1.

dummy(Arg1, Arg2, Arg3) ->
    {Arg1, Arg2, Arg3}.

passthrough_test() ->
    ?assert(true).

% These tests trigger the compiler error handling, and prevent the
% rest of the tests from running.
%
% cut_argument_compile_error_test() ->
%     partial:cut(atom),
%     partial:cut(partial:cut(atom)),
%     partial:cut(dummy(), dummy()).
% 
% cute_argument_compile_error_test() ->
%     partial:cute(atom),
%     partial:cute(partial:cute(atom)),
%     partial:cute(dummy(), dummy()).

cut_with_no_args_test() ->
    Partial = partial:cut(dummy()),
    ?assert(Partial()).

cut_with_no_cuts_test() ->
    Partial = partial:cut(dummy(1, 2, 3)),
    ?assertEqual({1, 2, 3}, Partial()).

cut_evaluation_time_test() ->
    Partial = partial:cut(
                dummy(
                  erlang:unique_integer())),
    ?assertNotEqual(Partial(), Partial()).

cut_capture_test() ->
    Value = 1,
    Partial = partial:cut(dummy(Value, 2, 3)),
    ?assertEqual({1, 2, 3}, Partial()).

cut_with_all_cuts_test() ->
    Partial = partial:cut(dummy(_, _, _)),
    ?assertEqual({1, 2, 3}, Partial(1, 2, 3)).

cut_function_name_test() ->
    Partial = partial:cut(_(1, 2, 3)),
    ?assertEqual({1, 2, 3}, Partial(fun dummy/3)).

cut_module_function_name_test() ->
    Partial = partial:cut(_:_(1, 2, 3)),
    ?assertEqual({1, 2, 3}, Partial(test_partial, dummy)).

cut_test() ->
    Partial = partial:cut(dummy(1, _, 3)),
    ?assertEqual({1, 2, 3}, Partial(2)).

cut_local_test() ->
    Partial = cut(dummy(1, _, 3)),
    ?assertEqual({1, 2, 3}, Partial(2)).

cute_with_no_args_test() ->
    Partial = partial:cute(dummy()),
    ?assert(Partial()).

cute_with_no_cuts_test() ->
    Partial = partial:cute(dummy(1, 2, 3)),
    ?assertEqual({1, 2, 3}, Partial()).

cute_evaluation_time_test() ->
    Partial = partial:cute(
                dummy(
                  erlang:unique_integer())),
    ?assertEqual(Partial(), Partial()).

cute_capture_test() ->
    Value = 1,
    Partial = partial:cute(dummy(Value, 2, 3)),
    ?assertEqual({1, 2, 3}, Partial()).

cute_with_all_cuts_test() ->
    Partial = partial:cute(dummy(_, _, _)),
    ?assertEqual({1, 2, 3}, Partial(1, 2, 3)).

cute_function_name_test() ->
    Partial = partial:cute(_(1, 2, 3)),
    ?assertEqual({1, 2, 3}, Partial(fun dummy/3)).

cute_module_function_name_test() ->
    Partial = partial:cute(_:_(1, 2, 3)),
    ?assertEqual({1, 2, 3}, Partial(test_partial, dummy)).

cute_test() ->
    Partial = partial:cute(dummy(1, _, 3)),
    ?assertEqual({1, 2, 3}, Partial(2)).

cute_local_test() ->
    Partial = cute(dummy(1, _, 3)),
    ?assertEqual({1, 2, 3}, Partial(2)).
