% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(nifoc_ct_helper).

-export([
  application_dir/1,
  priv_dir/1,
  test_dir/1,
  test_modules/2,
  eunit_modules/1,
  eqc_modules/1,
  triq_modules/1
]).

-spec application_dir(module()) -> string().
application_dir(Mod) ->
  Ebin = filename:dirname(code:which(Mod)),
  Root = filename:dirname(Ebin),
  case string:substr(Root, 1, 1) of
    "." ->
      {ok, Cwd} = file:get_cwd(),
      EunitTest = string:str(Cwd, ".eunit") > 0,
      CommonTest = string:str(Cwd, "ct_run") > 0,
      if
        EunitTest -> filename:dirname(Cwd);
        CommonTest -> filename:dirname(filename:dirname(Cwd));
        true -> Root
      end;
    _ -> Root
  end.

-spec priv_dir(module()) -> string().
priv_dir(Mod) ->
  Root = application_dir(Mod),
  filename:join(Root, "priv").

-spec test_dir(module()) -> string().
test_dir(Mod) ->
  Root = application_dir(Mod),
  filename:join(Root, "test").

-spec test_modules(module(), string()) -> [module()].
test_modules(Mod, Ext) ->
  {ok, Files} = file:list_dir(test_dir(Mod)),
  Files2 = lists:filter(fun(File) ->
    case re:run(File, Ext ++ "\\.erl$") of
      {match, _} -> true;
      nomatch -> false
    end
  end, Files),
  [list_to_atom(re:replace(File, "\\.erl$", "", [{return, list}])) || File <- Files2].

-spec eunit_modules(module()) -> [module()].
eunit_modules(Mod) ->
  test_modules(Mod, "_test").

-spec eqc_modules(module()) -> [module()].
eqc_modules(Mod) ->
  test_modules(Mod, "_eqc").

-spec triq_modules(module()) -> [module()].
triq_modules(Mod) ->
  test_modules(Mod, "_triq").
