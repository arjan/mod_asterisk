%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-10-10

%% @doc Asterisk module for handling incoming and outgoing phone calls.

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

-module(mod_asterisk).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("Asterisk").
-mod_description("Asterisk module for handling incoming and outgoing phone calls.").
-mod_prio(900).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-include_lib("include/zotonic.hrl").

-record(state, {context}).

start_link(Args) when is_list(Args) ->
    ?DEBUG("DOOO"),
    gen_server:start_link(?MODULE, Args, []).


init(Args) ->
    {context, Context} = proplists:lookup(context, Args),

    ?DEBUG("Starting asterisk module"),
    application:load(eastrisk),
    application:set_env(eastrisk, mgr_host, z_convert:to_list(m_config:get_value(?MODULE, mgr_host, "localhost", Context))),
    application:set_env(eastrisk, mgr_port, z_convert:to_integer(m_config:get_value(?MODULE, mgr_port, "5038", Context))),
    application:set_env(eastrisk, mgr_name, z_convert:to_list(m_config:get_value(?MODULE, mgr_name, "asterisk", Context))),
    application:set_env(eastrisk, mgr_secret, z_convert:to_list(m_config:get_value(?MODULE, mgr_secret, "asterisk", Context))),

    application:set_env(eastrisk, agi_port, z_convert:to_integer(m_config:get_value(?MODULE, agi_port, "6666", Context))),

    case application:start(eastrisk) of
        ok ->
            {ok, #state{context=Context}};
        {error, _Reason} ->
            z_session_manager:broadcast(#broadcast{type="error", message="Asterisk running in different context!", title="Asterisk", stay=true}, z_acl:sudo(Context)),
            ignore
    end.



handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ?DEBUG(1111111111),
    ?DEBUG("bye"),
    ok = application:stop(eastrisk).

