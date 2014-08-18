% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(nifoc_ct_hook).

-export([
  id/1,
  init/2,
  pre_init_per_testcase/3,
  post_end_per_testcase/4
]).

id(_Options) -> ?MODULE.

init(_Id, _Options) ->
  {ok, []}.

pre_init_per_testcase(TC, Config, State) when TC =:= eunit; TC =:= eqc ->
  ok = ct:capture_start(),
  {Config, State};
pre_init_per_testcase(_TC, Config, State) ->
  {Config, State}.

post_end_per_testcase(TC, _Config, Return, State) when TC =:= eunit; TC =:= eqc ->
  case Return of
    {error, _} ->
      Output = lists:droplast(ct:capture_get()),
      io:format(user, "~s~n", [lists:flatten(Output)]);
    _ -> ok
  end,
  ok = ct:capture_stop(),
  {Return, State};
post_end_per_testcase(_TC, _Config, Return, State) ->
  {Return, State}.
