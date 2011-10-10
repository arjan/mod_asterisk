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

-include_lib("include/zotonic.hrl").

-export([
         init/1
        ]).


init(_Context) ->
    ?DEBUG("Starting asterisk module"),
    ok.
