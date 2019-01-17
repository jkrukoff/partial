%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(partial).

-define(MATCH_REMOTE(Module, Name),
        {remote, _, {atom, _, Module}, {atom, _, Name}}).

%% API
-export([cut/1,
         cute/1,
         parse_transform/2]).

-include_lib("syntax_tools/include/merl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% A dummy function used as a marker by parse_transform/2 to convert:
%% 
%% F = partial:cut(f(_, x, y, z)).
%%
%% into:
%%
%% F = fun (Arg1) ->
%%     f(Arg1, x, y, z)
%% end.
%% @end
cut(_Fun) ->
    missing_parse_transform().

%% @doc
%% A dummy function used as a marker by parse_transform/2 to convert:
%% 
%% F = partial:cute(f(_, x, y, z)).
%%
%% into:
%%
%% F = (fun () ->
%%      Arg2 = x,
%%      Arg3 = y,
%%      Arg4 = z,
%%      fun (Arg1) ->
%%          f(Arg1, Arg2, Arg3, Arg4)
%%      end
%% end)().
%% @end
cute(_Fun) ->
    missing_parse_transform().

parse_transform(Forms, _Options) ->
    % ok = io:format("Forms = ~p~n", [Forms]),
    Transformed = transform_forms(Forms),
    % ok = io:format("Transformed Forms = ~p~n", [Transformed]),
    Transformed.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

missing_parse_transform() ->
    throw({missing_parse_transform,
           "This function requires that the partial:parse_trans/2 "
           "function be listed as a parse transformation for your "
           "module as \"-compile({parse_transform, partial}).\" "
           "If that is already true, this function has been called "
           "indirectly in a way the parse transform can not "
           "recognize, such as via apply/3."}).

transform_forms(Forms) ->
    % If any errors occured, we replace the entire form with them.
    % This allows the standard compiler chaing to pick them up as if
    % they were erl_parse errors and display a standard error message
    % to the user instead of a traceback.
    erl_syntax:revert_forms(
      lists:append(
          [case erl_syntax_lib:mapfold(fun transform/2, [], Form) of
               {Transformed, []} ->
                   [Transformed];
               {_Transformed, Errors} ->
                   Errors
           end || Form <- Forms])).

transform(Form, Errors) ->
    Line = erl_syntax:get_pos(Form),
    Transformed = case Form of
        ?Q("partial:cut(_@@Args)") ->
            io:format("Cut Args ~p~n", [Args]),
            Cut = cut_function(Line, Args),
            io:format("Cut Transform ~p~n", [Cut]),
            Cut;
        ?Q("partial:cute(_@@Args)") ->
            io:format("Cute Args ~p~n", [Args]),
            Cute = cute_function(Line, Args),
            io:format("Cute Transform ~p~n", [Cute]),
            Cute;
        _ ->
            {ok, Form}
    end,
    case Transformed of
        {ok, NewForm} ->
            {NewForm, Errors};
        {error, Error} ->
            {Form, [Error | Errors]}
    end.

transform_error(Line, Message) ->
    % I'm probably abusing the error system by using erl_parse here,
    % but it allows my errors to show up as usual in the compiler
    % output.
    Prefix = "Error: In partial:parse_transform/2, ",
    {error, erl_syntax:error_marker({Line, erl_parse, Prefix ++ Message})}.

cut_function(MarkerLine, [MarkerArgument])->
    case MarkerArgument of
        ?Q("_@Name(_@@Args)") ->
            Line = erl_syntax:get_pos(MarkerArgument),
            CutVariables = cut_variables(Line, Args),
            CutArguments = cut_arguments(Args, CutVariables),
            CutFun = erl_syntax:set_pos(
                       ?Q("fun (_@@CutVariables) ->"
                          " _@Name(_@@CutArguments)"
                          " end"),
                       Line),
            {ok, CutFun};
        _ ->
            transform_error(
              MarkerLine,
              "partial:cut/1 requires a function call as an argument")
    end;
cut_function(MarkerLine, MarkerArguments) ->
    transform_error(
      MarkerLine,
      io_lib:format(
        "partial:cut/1 requires a single argument, got ~b",
        [length(MarkerArguments)])).

cute_function(MarkerLine, [MarkerArgument])->
    case MarkerArgument of
        ?Q("_@Name(_@@Args)") ->
            Line = erl_syntax:get_pos(MarkerArgument),
            CutVariables = cut_variables(Line, Args),
            CuteMatches = cute_matches(Line, Args),
            CuteVariables = cute_variables(CuteMatches),
            CuteArguments = cute_arguments(Args, CutVariables, CuteVariables),
            CuteFun = erl_syntax:set_pos(
                        ?Q("(fun () ->"
                           " _@@CuteMatches,"
                           " fun (_@@CutVariables) -> _@Name(_@@CuteArguments) end"
                           " end)()"),
                        Line),
            {ok, CuteFun};
        _ ->
            transform_error(
              MarkerLine,
              "partial:cute/1 requires a function call as an argument")
    end;
cute_function(MarkerLine, MarkerArguments)->
    transform_error(
      MarkerLine,
      io_lib:format(
        "partial:cute/1 requires a single argument, got ~b",
        [length(MarkerArguments)])).

is_cut_variable(Var) ->
    case Var of
        ?Q("_") ->
            true;
        _ ->
            false
    end.

variable_name(Type) ->
    Name = io_lib:format("PartialArgument_~s_~w", [Type, make_ref()]),
    erlang:list_to_atom(lists:flatten(Name)).

cut_variables(Line, Args) ->
    [erl_syntax:set_pos(
       erl_syntax:variable(
         variable_name(cut)),
       Line) ||
     Arg <- Args,
     is_cut_variable(Arg)].

cut_arguments([], []) ->
    [];
cut_arguments([Arg | Args], Variables) ->
    case is_cut_variable(Arg) of
        true ->
            [hd(Variables) | cut_arguments(Args, tl(Variables))];
        false ->
            [Arg | cut_arguments(Args, Variables)]
    end.

cute_matches(Line, Args) ->
    [erl_syntax:set_pos(
       erl_syntax:match_expr(
           erl_syntax:set_pos(
             erl_syntax:variable(variable_name(cute)),
             Line),
           Arg),
       Line) ||
     Arg <- Args,
     not is_cut_variable(Arg)].

cute_variables(Matches) ->
    ExtractPattern = fun (Match) ->
        ?Q("_@Pattern = _@_") = Match,
        Pattern
    end,
    [ExtractPattern(Match) || Match <- Matches].

cute_arguments([], [], []) ->
    [];
cute_arguments([Arg | Args], CutVariables, CuteVariables) ->
    case is_cut_variable(Arg) of
        true ->
            [hd(CutVariables) | cute_arguments(Args,
                                               tl(CutVariables),
                                               CuteVariables)];
        false ->
            [hd(CuteVariables) | cute_arguments(Args,
                                                CutVariables,
                                                tl(CuteVariables))]
    end.
