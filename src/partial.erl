%%%-------------------------------------------------------------------
%%% @doc
%%% A parse transform implementing partial function application.
%%% @end
%%%-------------------------------------------------------------------
-module(partial).

% -define(PARTIAL_DEBUG, true).

-ifdef(PARTIAL_DEBUG).
-define(IF_DEBUG(Expression), Expression).
-else.
-define(IF_DEBUG(Expression), (ok)).
-endif.

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
%% A dummy function used as a marker by parse_transform/2 to convert
%% calls to functions to partially applied functions. The special
%% variable '_' is used as a marker for unevaluated arguments, as it
%% is usually illegal to use on the right hand side of a match.
%%
%% All arguments are evaluated when the partially applied function is
%% called.
%%
%% The parse transform is only able to detect and rewrite simple
%% literal calls to this function. Other uses will result in an error
%% being thrown at runtime.
%% @end
%% @see parse_transform/2
cut(_Fun) ->
    missing_parse_transform().

%% @doc
%% A dummy function used as a marker by parse_transform/2 to convert
%% calls to functions to partially applied functions. The special
%% variable '_' is used as a marker for unevaluated arguments, as it
%% is usually illegal to use on the right hand side of a match.
%%
%% Given arguments are evaluated when the partially applied function
%% is constructed. This can be used as an easy way to cache expensive
%% computation in a closure.
%%
%% The parse transform is only able to detect and rewrite simple
%% literal calls to this function. Other uses will result in an error
%% being thrown at runtime.
%% @end
%% @see parse_transform/2
cute(_Fun) ->
    missing_parse_transform().

%% @doc
%% A parse transformation function which converts calls to special
%% dummy functions in this module.
%%
%% Add:
%% -compile({parse_transform, partial}).
%%
%% to the top of any module to enable.
%% @end
parse_transform(Forms, _Options) ->
    transform_forms(Forms).

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
    % This allows the standard compiler chain to pick them up as if
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
            ?IF_DEBUG(ok = io:format(
                             "partial:parse_transform/2 cut Args ~p~n",
                             [Args])),
            Cut = cut_function(Line, Args),
            ?IF_DEBUG(ok = io:format(
                             "partial:parse_transform/2 cut Transform ~p~n",
                             [Cut])),
            Cut;
        ?Q("partial:cute(_@@Args)") ->
            ?IF_DEBUG(ok = io:format(
                             "partial:parse_transform/2 cute Args ~p~n",
                             [Args])),
            Cute = cute_function(Line, Args),
            ?IF_DEBUG(ok = io:format(
                             "partial:parse_transform/2 cute Transform ~p~n",
                             [Cute])),
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

%% @doc
%% Transform the AST for:
%% 
%% F = partial:cut(f(_, x, y, z)).
%%
%% into:
%%
%% F = fun (Arg1) ->
%%     f(Arg1, x, y, z)
%% end.
%% @end
cut_function(MarkerLine, [MarkerArgument])->
    case MarkerArgument of
        ?Q("_@Name(_@@Args)") ->
            Line = erl_syntax:get_pos(MarkerArgument),
            CutVariables = cut_variables(Line, Args),
            CutArguments = cut_arguments(Args, CutVariables),
            CutFun = merl:qquote(Line,
                                 "fun (_@@variables) ->"
                                 " _@name(_@@arguments)"
                                 " end",
                                 [{variables, CutVariables},
                                  {name, Name},
                                  {arguments, CutArguments}]),
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

%% @doc
%% Transform the AST for:
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
cute_function(MarkerLine, [MarkerArgument])->
    case MarkerArgument of
        ?Q("_@Name(_@@Args)") ->
            Line = erl_syntax:get_pos(MarkerArgument),
            CutVariables = cut_variables(Line, Args),
            CuteMatches = cute_matches(Line, Args),
            CuteVariables = cute_variables(CuteMatches),
            CuteArguments = cute_arguments(Args, CutVariables, CuteVariables),
            CuteFun = merl:qquote(Line,
                                  "(fun () ->"
                                  " _@@matches,"
                                  " fun (_@@variables) -> _@name(_@@arguments) end"
                                  " end)()",
                                  [{matches, CuteMatches},
                                   {variables, CutVariables},
                                   {name, Name},
                                   {arguments, CuteArguments}]),
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

variable(Line, Type) ->
    erl_syntax:set_pos(erl_syntax:variable(variable_name(Type)), Line).

cut_variables(Line, Args) ->
    [variable(Line, cut) || Arg <- Args, is_cut_variable(Arg)].

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
    MatchExpr = fun (Arg) -> 
        Pattern = variable(Line, cute),
        merl:qquote(Line,
                    "_@pattern = _@arg",
                    [{pattern, Pattern}, {arg, Arg}])
    end,
    [MatchExpr(Arg) || Arg <- Args, not is_cut_variable(Arg)].

cute_variables(Matches) ->
    lists:map(
      fun (Match) -> ?Q("_@Pattern = _@_") = Match, Pattern end,
      Matches).

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
