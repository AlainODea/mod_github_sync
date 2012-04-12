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
         process_post/2]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) ->
    {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

%% only POST requests are valid for PayPal IPN
allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    validate_ip(ReqData, Context, m_config:get_value(mod_github_sync, accepted_ips, Context)).

validate_ip(ReqData, Context, undefined) ->
    validate_token(ReqData, Context, m_config:get_value(mod_github_sync, accepted_tokens, Context));
validate_ip(ReqData, Context, AcceptedIPs) when is_binary(AcceptedIPs) ->
    validate_ip(ReqData, Context, binary_to_list(AcceptedIPs));
validate_ip(ReqData, Context, AcceptedIPs) ->
    check_ip(ReqData, Context, string:str(AcceptedIPs, wrq:peer(ReqData))).

check_ip(ReqData, Context, 0) ->
    {false, ReqData, Context};
check_ip(ReqData, Context, _) ->
    validate_token(ReqData, Context, m_config:get_value(mod_github_sync, accepted_token, Context)).

validate_token(ReqData, Context, undefined) ->
    {true, ReqData, Context};
validate_token(ReqData, Context, AcceptedToken) when is_binary(AcceptedToken) ->
    validate_token(ReqData, Context, binary_to_list(AcceptedToken));
validate_token(ReqData, Context, AcceptedToken) ->
    check_token(ReqData, Context, AcceptedToken =:= token(Context, ReqData)).

check_token(ReqData, Context, false) ->
    {false, ReqData, Context};
check_token(ReqData, Context, true) ->
    {true, ReqData, Context}.

token(Context, ReqData) ->
    Context1 = z_context:set_reqdata(ReqData, Context),
    ContextQs = z_context:ensure_qs(Context1),
    get_token(ContextQs, z_context:get(token, ContextQs)).

get_token(ContextQs, undefined) ->
    z_context:get_q("token", ContextQs);
get_token(_ContextQs, Token) ->
    Token.

process_post(ReqData, Context) ->
    {true, ReqData, update(Context)}.

%% Internal API
update(Context) ->
    Site = z_context:site(Context),
    run(update_command(vcs_root(Site))),
    Context.

run(ok) ->
    ok;
run(Command) ->
    spawn(fun() ->
        os:cmd(lists:flatten(Command)),
        z:m()
    end).

update_command({hg, Path}) ->
    ["(cd \"", Path, "\"; hg pull -u)"];
update_command({git, Path}) ->
    ["(cd \"", Path, "\"; git pull)"];
update_command(undefined) ->
    ok.

%% Code robbed and tuned from mod_zotonic_status_vcs, needs refactor
vcs_root(Site) ->
    vcs_root_dir(site_path(Site)).

vcs_root_dir(Dir) ->
    HgDir = filename:join([Dir, ".hg"]),
    hg_root(filelib:is_dir(HgDir), Dir).

hg_root(true, Dir) ->
    {hg, Dir};
hg_root(false, Dir) ->
    GitDir = filename:join([Dir, ".git"]),
    git_root(filelib:is_dir(GitDir), Dir).

git_root(true, Dir) ->
    {git, Dir};
git_root(false, _) ->
    undefined.

site_path(Site) ->
    filename:join([z_utils:lib_dir(priv), "sites", Site]).
