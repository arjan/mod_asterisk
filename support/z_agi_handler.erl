%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-10-10

%% @doc AGI handler. Simply converts gen_event new_channel
%% notification into a Zotonic #asterisk_incoming{} notification and
%% broadcasts it to the first receiving party.

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

-module(z_agi_handler).
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
-export([agi_answer/3]).

-include("zotonic.hrl").
-include("../deps/eastrisk/include/agi.hrl").
-include("../include/z_asterisk.hrl").

%%% gen_event callbacks
init(Context) -> {ok, Context}.

%% @doc Handle the 'new channel' event. This is the only AGI event.
handle_event({new_channel, Pid, Env}, Context) ->
	% Pick up any call that has been connected to the AGI entry in the dialplan.
	spawn(?MODULE, agi_answer, [Pid, Env, Context]),
	{ok, Context}.

handle_call(_, State) -> {stop, not_supported, State}.
handle_info(_, State) -> {ok, State}.

terminate(_Reason, State) ->
	State.

code_change(_OldVsn, State, _Extra) ->
	{stop, not_supported, State}.

%%% Internal functions
agi_answer(ChPid, AgiEnv, Context) ->
    case z_notifier:first(#asterisk_incoming{channel_pid=ChPid, environment=AgiEnv}, Context) of
        undefined ->
            %% No module answered the call
            ?zInfo("mod_asterisk: Incoming call but no handling module!", Context),
            agi:answer(ChPid),
            agi:hangup(ChPid);
        ok ->
            nop
    end,
    agi_channel:close(ChPid).
