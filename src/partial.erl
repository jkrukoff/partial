%%%-------------------------------------------------------------------
%%% @doc
%%% A parse transform implementing partial function application.
%%%
%%% To enable, add to the top of your module:
%%%
%%% ```
%%% -compile({parse_transform, partial}).
%%% '''
%%%
%%% This will enable compile time conversion of calls to
%%% `partial:cut/1' and `partial:cute/1' into partial function
%%% application of the contained function. `_' is used as a marker for
%%% the unevaluated slot(s) in the contained function.
%%%
%%% With `partial:cut/1', the arguments to the called function are
%%% evaluated when the returned function is applied. With
%%% `partial:cute/1', the arguments are evaluated when the function is
%%% constructed.
%%%
%%% Additionally, a compile option can be specified via erlc options
%%% or by adding to the top of your module:
%%%
%%% ```
%%% -compile(partial_allow_local).
%%% '''
%%%
%%% To enable transforming `cut/1' and `cute/1' the same as the fully
%%% qualified names.
%%% @end
%%%-------------------------------------------------------------------
-module(partial).

% Manually enable debugging. Should usually just use a compiler flag,
% though. Is quite verbose.
% -define(PARTIAL_DEBUG, true).

-ifdef(PARTIAL_DEBUG).
-define(IF_DEBUG(Expression), Expression).
-else.
-define(IF_DEBUG(Expression), (ok)).
-endif.

%% API
-export([cut/1,
         cute/1,
         parse_transform/2]).

-record(cut, {variables=[], arguments=[]}).
-record(cute, {variables=[], matches=[], arguments=[]}).

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
-spec parse_transform(Forms, Options) -> NewForms when
      Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
      Options :: [compile:option()],
      NewForms :: [erl_parse:abstract_form() | erl_parse:form_info()].
parse_transform(Forms, Options) ->
    GlobalOptions = parse_options(Options),
    FileOptions = parse_compile_attributes(Forms),
    MergedOptions = merge_options(GlobalOptions, FileOptions),
    ?IF_DEBUG(ok = io:format(
                     "partial:parse_transform/2 options:~n~p~n",
                     [MergedOptions])),
    transform_forms(Forms, MergedOptions).

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

parse_options(Options) ->
    ?IF_DEBUG(ok = io:format(
                     "partial:parse_options/2 options:~n~p~n",
                     [Options])),
    AllowLocal = proplists:get_value(partial_allow_local, Options),
    ok = case lists:member(AllowLocal, [true, false, undefined]) of
             true ->
                 ok;
             false ->
                 Message = lists:flatten(
                             io_lib:format(
                               "Value for compiler option partial_allow_local "
                               "must be a boolean, but value was: ~w",
                               [AllowLocal])),
                 error({invalid_compile_option, Message})
         end,
    #{allow_local=>AllowLocal}.

compile_attributes(Forms) ->
    Attributes = [begin
                      Attribute = erl_syntax:concrete(Args),
                      case is_list(Attribute) of
                          true ->
                              Attribute;
                          false ->
                              [Attribute]
                      end
                  end || 
                  Form <- Forms,
                  {match, Args} <- [case Form of
                                        ?Q("-compile('@Args').") ->
                                            {match, Args};
                                        _ ->
                                            undefined
                                    end]],
    lists:append(Attributes).

parse_compile_attributes(Forms) ->
    Attributes = compile_attributes(Forms),
    parse_options(Attributes).

%% @private
%% @doc
%% We parse options from the compiler and from the file being
%% compiled. If an option is only given in one place, use that. If
%% given in both places the file overrides the global option.
%% @end
-spec merge_options(GlobalOptions, FileOptions) -> Options when
      GlobalOptions :: #{allow_local:=undefined | boolean()},
      FileOptions :: #{allow_local:=undefined | boolean()},
      Options :: #{allow_local:=boolean()}.
merge_options(#{allow_local:=undefined}, #{allow_local:=undefined}) ->
    #{allow_local=>false};
merge_options(#{allow_local:=undefined}, #{allow_local:=true}) ->
    #{allow_local=>true};
merge_options(#{allow_local:=undefined}, #{allow_local:=false}) ->
    #{allow_local=>false};
merge_options(#{allow_local:=true}, #{allow_local:=undefined}) ->
    #{allow_local=>true};
merge_options(#{allow_local:=false}, #{allow_local:=undefined}) ->
    #{allow_local=>false};
merge_options(#{allow_local:=true}, #{allow_local:=true}) ->
    #{allow_local=>true};
merge_options(#{allow_local:=true}, #{allow_local:=false}) ->
    #{allow_local=>false};
merge_options(#{allow_local:=false}, #{allow_local:=true}) ->
    #{allow_local=>true};
merge_options(#{allow_local:=false}, #{allow_local:=false}) ->
    #{allow_local=>false}.

transform_forms(Forms, Options) ->
    % If any errors occured, we replace the entire form with them.
    % This allows the standard compiler chain to pick them up as if
    % they were erl_parse errors and display a standard error message
    % to the user instead of a traceback.
    Transform = fun (Form, Errors) ->
                        transform(Form, Errors, Options)
                end,
    erl_syntax:revert_forms(
      lists:append(
        [case erl_syntax_lib:mapfold(Transform, [], Form) of
             {Transformed, []} ->
                 [Transformed];
             {_Transformed, Errors} ->
                 Errors
         end || Form <- Forms])).

transform(Form, Errors, Options) ->
    Line = erl_syntax:get_pos(Form),
    Transformed = case Form of
                      ?Q("partial:cut(_@@Args)") ->
                          cut_function(Line, Args);
                      ?Q("cut(_@@Args)") ->
                          case Options of
                              #{allow_local:=true} ->
                                  cut_function(Line, Args);
                              _ ->
                                  {ok, Form}
                          end;
                      ?Q("partial:cute(_@@Args)") ->
                          cute_function(Line, Args);
                      ?Q("cute(_@@Args)") ->
                          case Options of
                              #{allow_local:=true} ->
                                  cute_function(Line, Args);
                              _ ->
                                  {ok, Form}
                          end;
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

reverse(#cut{} = Cut) ->
    #cut{variables=lists:reverse(Cut#cut.variables),
         arguments=lists:reverse(Cut#cut.arguments)};
reverse(#cute{} = Cute) ->
    #cute{variables=lists:reverse(Cute#cute.variables),
          matches=lists:reverse(Cute#cute.matches),
          arguments=lists:reverse(Cute#cute.arguments)}.

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

name([Name]) ->
    [Name];
name([Module, Function]) ->
    ?Q("_@Module:_@Function").

split_name(Name) ->
    case Name of
        ?Q("_@Module:_@Function") ->
            [Module, Function];
        _ ->
            [Name]
    end.

match(Line, Type, Form) ->
    Pattern = variable(Line, Type),
    Match = merl:qquote(Line,
                        "_@pattern = _@form",
                        [{pattern, Pattern}, {form, Form}]),
    {Pattern, Match}.

cuts(Line, Type, Forms) ->
    reverse(lists:foldl(
              fun (Form, #cut{} = Acc) ->
                      case is_cut_variable(Form) of
                          true ->
                              Variable = variable(Line, Type),
                              Acc#cut{variables=[Variable | Acc#cut.variables],
                                      arguments=[Variable | Acc#cut.arguments]};
                          false ->
                              Acc#cut{arguments=[Form | Acc#cut.arguments]}
                      end
              end,
              #cut{},
              Forms)).

name_cuts(Line, Name) ->
    Parts = split_name(Name),
    Cut = cuts(Line, name, Parts),
    Cut#cut{arguments=name(Cut#cut.arguments)}.

cutes(Line, Type, Forms) ->
    reverse(lists:foldl(
              fun (Form, #cute{} = Acc) ->
                      case {is_cut_variable(Form), erl_syntax:is_literal(Form)} of
                          {true, false} ->
                              Variable = variable(Line, Type),
                              Acc#cute{variables=[Variable | Acc#cute.variables],
                                       arguments=[Variable | Acc#cute.arguments]};
                          {false, true} ->
                              Acc#cute{arguments=[Form | Acc#cute.arguments]};
                          {false, false} ->
                              {Variable, Match} = match(Line, Type, Form),
                              Acc#cute{matches=[Match | Acc#cute.matches],
                                       arguments=[Variable | Acc#cute.arguments]}
                      end
              end,
              #cute{},
              Forms)).

name_cutes(Line, Name) ->
    Parts = split_name(Name),
    Cute = cutes(Line, name, Parts),
    Cute#cute{arguments=name(Cute#cute.arguments)}.

%% @private
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
            ?IF_DEBUG(ok = io:format(
                             "partial:cut_function/2 Original:~n~p~n",
                             [MarkerArgument])),
            Line = erl_syntax:get_pos(MarkerArgument),
            NameCut = name_cuts(Line, Name),
            Cut = cuts(Line, cut, Args),
            CutFun = merl:qquote(Line,
                                 "fun (_@@variables) ->"
                                 " _@name(_@@arguments)"
                                 " end",
                                 [{variables, NameCut#cut.variables ++ Cut#cut.variables},
                                  {name, NameCut#cut.arguments},
                                  {arguments, Cut#cut.arguments}]),
            ?IF_DEBUG(ok = io:format(
                             "partial:cut_function/2 Transformed:~n~p~n",
                             [CutFun])),
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

%% @private
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
            ?IF_DEBUG(ok = io:format(
                             "partial:cute_function/2 Original:~n~p~n",
                             [MarkerArgument])),
            Line = erl_syntax:get_pos(MarkerArgument),
            NameCute = name_cutes(Line, Name),
            Cute = cutes(Line, cute, Args),
            CuteFun = merl:qquote(Line,
                                  "(fun () ->"
                                  " _@@matches,"
                                  " fun (_@@variables) -> _@name(_@@arguments) end"
                                  " end)()",
                                  [{matches, NameCute#cute.matches ++ Cute#cute.matches},
                                   {variables, NameCute#cute.variables ++ Cute#cute.variables},
                                   {name, NameCute#cute.arguments},
                                   {arguments, Cute#cute.arguments}]),
            ?IF_DEBUG(ok = io:format(
                             "partial:cute_function/2 Transformed:~n~p~n",
                             [CuteFun])),
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
