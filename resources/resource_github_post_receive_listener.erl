%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2012 Alain O'Dea
%% @date 2012-04-09
%% @doc Webmachine-based Github Post-Receive Hook
%% The role of the resource_github_post_receive_listener is to receive POSTs
%% from Github and trigger a pull and rebuild of site code
%% @end

%% Copyright 2012 Alain O'Dea
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

-module(resource_github_post_receive_listener).
-author("Alain O'Dea <alain.odea@gmail.com>").
-export([init/1]).
%% resource functions
-export([service_available/2,
         allowed_methods/2,
         resource_exists/2,
         process_post/2,
         finish_request/2]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) ->
    {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

%% only POST requests are valid for Post-Receive Hooks
allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    % play dead to hide from hackers
    ?WM_REPLY(false, Context1).

finish_request(ReqData, Context) ->
    % Do the actual processing after response to avoid slow 404
    % which makes the ruse kind of obvious to keen hackers
    Context1 = ?WM_REQ(ReqData, Context),
    ContextQs = z_context:ensure_qs(Context1),
    [_|_] = z_context:get_q("payload", ContextQs),
    {true, ReqData, Context}.
