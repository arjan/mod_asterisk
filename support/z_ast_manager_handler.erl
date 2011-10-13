%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-10-10

%% @doc AST manager events handler

%% Copyright 2011 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_ast_manager_handler).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-behaviour(gen_event).

-export([init/1]).
-export([
	handle_event/2,
	terminate/2,
	code_change/3,
	handle_call/2,
	handle_info/2
]).

-include("zotonic.hrl").
-include("../include/z_asterisk.hrl").

%%% gen_event callbacks
init(Context) -> {ok, Context}.

%% @doc Just passes the gen_event notification in to zotonic' notifier system.
handle_event(Event, Context) ->
    z_notifier:first(#asterisk_event{event=Event}, Context),
	{ok, Context}.

handle_call(_, State) -> {stop, not_supported, State}.
handle_info(_, State) -> {ok, State}.

terminate(_Reason, State) ->
	State.

code_change(_OldVsn, State, _Extra) ->
	{stop, not_supported, State}.
