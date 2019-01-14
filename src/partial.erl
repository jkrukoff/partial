%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(partial).

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
    ok = io:format("Forms = ~p~n", [Forms]),
    Transformed = [transform_maybe_error(Form) || Form <- Forms],
    ok = io:format("Transformed Forms = ~p~n", [Transformed]),
    Transformed.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

missing_parse_transform() ->
    throw({missing_parse_transform,
           "This function requires that the partial:parse_trans/2 "
           "function be listed as a parse transformation for your "
           "module as \"-compile({parse_transform, partial}).\""
           "If that is true, this function has been called "
           "indirectly in a way the parse transform can not "
           "recognize, such as via apply/3."}).

transform_maybe_error(Form) ->
    % Parse errors can only appear as top level forms. When an error
    % occurs parsing a partial marker function, we throw an exception
    % and report it at a level the compiler can cope with.
    % Unfortunately, this only allows reporting on the first error
    % found in a given form.
    [Transformed] = try transform([Form])
                    catch
                        {transform_error, What} ->
                            [{error, What}]
                    end,
    Transformed.

transform_error(Line, Message) ->
    % I'm probably abusing the error system by using erl_parse here,
    % but it allows my errors to show up as usual in the compiler
    % output.
    throw({transform_error, {Line, erl_parse, Message}}).

transform([]) ->
    [];
transform([{call, Line, Name, Args} = Form | Forms]) ->
    Transformed = case Name of
        {remote, _, {atom, _, partial}, {atom, _, cut}} ->
            ok = io:format("Found cut: ~w~n", [Args]),
            cut_function(Line, transform(Args));
        {remote, _, {atom, _, partial}, {atom, _, cute}} ->
            ok = io:format("Found cute: ~w~n", [Args]),
            cute_function(Line, transform(Args));
        _ ->
            list_to_tuple(transform(tuple_to_list(Form)))
    end,
    [Transformed | transform(Forms)];
transform([Form | Forms]) when is_atom(element(1, Form)) ->
    [list_to_tuple(transform(tuple_to_list(Form))) | transform(Forms)];
transform([List | Forms]) when is_list(List) ->
    [transform(List) | transform(Forms)];
transform([Form | Forms]) ->
    [Form | transform(Forms)];
transform(Form) ->
    Form.

is_cut_variable({var, _Line, '_'}) -> true;
is_cut_variable(_) -> false.

unique() ->
    erlang:unique_integer([monotonic, positive]).

variable_name(Type, Line) ->
    Name = io_lib:format("PartialArgument_~s_~b_~b", [Type, Line, unique()]),
    erlang:list_to_atom(Name).

cut_function(_MarkerLine, [{call, Line, Name, Args}]) ->
    CutVariables = cut_variables(Line, Args),
    CutArguments = cut_arguments(Args, CutVariables),
    {'fun', Line,
     {clauses,
      [{clause, Line,
        CutVariables,
        [],
        [{call, Line,
          Name,
          CutArguments}]}]}};
cut_function(MarkerLine, _) ->
    transform_error(
      MarkerLine,
      "Error: partial:cut/1 requires a single function call as an argument").

cute_function(_MarkerLine, [{call, Line, Name, Args}]) ->
    CutVariables = cut_variables(Line, Args),
    CuteMatches = cute_matches(Line, Args),
    CuteVariables = cute_variables(CuteMatches),
    CuteArguments = cute_arguments(Args, CutVariables, CuteVariables),
    {call, Line,
     {'fun', Line,
      {clauses,
       [{clause, Line,
         [],
         [],
         CuteMatches ++
         [{'fun', Line,
           {clauses,
            [{clause, Line,
              CutVariables,
              [],
              [{call, Line,
                Name,
                CuteArguments}]}]}}]}]}},
     []};
cute_function(MarkerLine, _) ->
    transform_error(
      MarkerLine,
      "Error: partial:cute/1 requires a single function call as an argument").

cut_variables(Line, Args) ->
    [{var, Line, variable_name(cut, Line)} || Arg <- Args, is_cut_variable(Arg)].

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
    [{match, Line, {var, Line, variable_name(cute, Line)}, Arg} ||
     Arg <- Args,
     not is_cut_variable(Arg)].

cute_variables(Matches) ->
    [Variable || {match, _, Variable, _} <- Matches].

cute_arguments([], [], []) ->
    [];
cute_arguments([Arg | Args], CutVariables, CuteVariables) ->
    case is_cut_variable(Arg) of
        true ->
            [hd(CutVariables) | cute_arguments(Args, tl(CutVariables), CuteVariables)];
        false ->
            [hd(CuteVariables) | cute_arguments(Args, CutVariables, tl(CuteVariables))]
    end.
