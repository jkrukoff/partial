

# Module partial #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A parse transform implementing partial function application.

<a name="description"></a>

## Description ##

To enable, add to the top of your module:

```
   -compile({parse_transform, partial}).
```

This will enable compile time conversion of calls to
`partial:cut/1` and `partial:cute/1` into partial function
application of the contained function. `_` is used as a marker for
the unevaluated slot(s) in the contained function.

With `partial:cut/1`, the arguments to the called function are
evaluated when the returned function is applied. With
`partial:cute/1`, the arguments are evaluated when the function is
constructed.

Additionally, a compile option can be specified via erlc options
or by adding to the top of your module:

```
   -compile(partial_allow_local).
```

To enable transforming `cut/1` and `cute/1` the same as the fully
qualified names.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cut-1">cut/1</a></td><td>
A dummy function used as a marker by parse_transform/2 to convert
calls to functions to partially applied functions.</td></tr><tr><td valign="top"><a href="#cute-1">cute/1</a></td><td>
A dummy function used as a marker by parse_transform/2 to convert
calls to functions to partially applied functions.</td></tr><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td>
A parse transformation function which converts calls to special
dummy functions in this module.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cut-1"></a>

### cut/1 ###

`cut(Fun) -> any()`

A dummy function used as a marker by parse_transform/2 to convert
calls to functions to partially applied functions. The special
variable '_' is used as a marker for unevaluated arguments, as it
is usually illegal to use on the right hand side of a match.

All arguments are evaluated when the partially applied function is
called.

The parse transform is only able to detect and rewrite simple
literal calls to this function. Other uses will result in an error
being thrown at runtime.

__See also:__ [parse_transform/2](#parse_transform-2).

<a name="cute-1"></a>

### cute/1 ###

`cute(Fun) -> any()`

A dummy function used as a marker by parse_transform/2 to convert
calls to functions to partially applied functions. The special
variable '_' is used as a marker for unevaluated arguments, as it
is usually illegal to use on the right hand side of a match.

Given arguments are evaluated when the partially applied function
is constructed. This can be used as an easy way to cache expensive
computation in a closure.

The parse transform is only able to detect and rewrite simple
literal calls to this function. Other uses will result in an error
being thrown at runtime.

__See also:__ [parse_transform/2](#parse_transform-2).

<a name="parse_transform-2"></a>

### parse_transform/2 ###

<pre><code>
parse_transform(Forms, Options) -&gt; NewForms
</code></pre>

<ul class="definitions"><li><code>Forms = [<a href="erl_parse.md#type-abstract_form">erl_parse:abstract_form()</a> | <a href="erl_parse.md#type-form_info">erl_parse:form_info()</a>]</code></li><li><code>Options = [<a href="compile.md#type-option">compile:option()</a>]</code></li><li><code>NewForms = [<a href="erl_parse.md#type-abstract_form">erl_parse:abstract_form()</a> | <a href="erl_parse.md#type-form_info">erl_parse:form_info()</a>]</code></li></ul>

A parse transformation function which converts calls to special
dummy functions in this module.

Add:
-compile({parse_transform, partial}).

to the top of any module to enable.

