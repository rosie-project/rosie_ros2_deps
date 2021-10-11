-module(rosie_ros2_resource).
-behaviour(rebar_resource_v2).
-export([init/2,
         lock/2,
         download/4, download/3,
         needs_update/2,
         make_vsn/1,
         make_vsn/2]).

-include_lib("xmerl/include/xmerl.hrl").

% -define(FOXY_DISTRO, "rosdistro/foxy/distribution.yaml").

download_distro() ->
  {ok, Download} = rebar_utils:sh("wget -q -O /dev/stdout https://raw.githubusercontent.com/ros/rosdistro/master/foxy/distribution.yaml",
    [{use_stdout, false}, {debug_abort_on_error, "FUCK canot retrieve distro file from github.. :("}]),
  application:start(yamerl),
  yamerl_constr:string(Download).
  

%% Initialize the custom dep resource plugin
init(Type, _RebarState) ->
  CustomState = #{foxy_distro => download_distro()},
  % CustomState=#{},
  Resource = rebar_resource_v2:new(
       Type,         % type tag such as 'git' or 'hg'
       ?MODULE,      % this callback module
       CustomState   % anything you want to carry around for next calls
  ),
  {ok, Resource}.

lock(AppInfo, CustomState) ->
  %io:format("WHAT LOCK?!\n"),
  %% Extract info such as {Type, ResourcePath, ...} as declared
  %% in rebar.config
  
  %SourceTuple = rebar_git_subdir_resource:lock(modify_app_info_for_git(AppInfo), CustomState),
  
  %% Annotate and modify the source tuple to make it absolutely
  %% and indeniably unambiguous (for example, with git this means
  %% transforming a branch name into an immutable ref)

  %% Return the unambiguous source tuple
  rebar_app_info:source(AppInfo).

download(TmpDir, AppInfo, RebarState) ->
  io:format("WHAT ???????????????\n"),
        %% Extract info such as {Type, ResourcePath, ...} as declared
        %% in rebar.config
        %rebar_git_resource:download(TmpDir, modify_app_info_for_git(AppInfo,CustomState), RebarState),
        %% Download the resource defined by SourceTuple, which should be
        %% an OTP application or library, into TmpDir
        ok.

download(TmpDir, AppInfo, RebarState, CustomState) ->
  case modify_app_info_for_git(AppInfo,CustomState) of
    skip_dependancy ->  rebar_api:warn("Skipping this dependancy",[]), ok;
    ModAppInfo ->
      case rebar_git_resource:download(TmpDir, ModAppInfo, RebarState, CustomState) of
        ok -> convert_repo_to_rebar3_project(TmpDir,AppInfo,CustomState), 
              ok;
        {error, Reason} -> rebar_api:error("Git dep failed with error: ~p",[Reason]), {error, Reason}
      end
    end.

make_vsn(Dir) ->
  io:format("WHAT vsn1\n"),
  %% Extract a version number from the application. This is useful
  %% when defining the version in the .app.src file as `{version, Type}',
  %% which means it should be derived from the build information. For
  %% the `git' resource, this means looking for the last tag and adding
  %% commit-specific information
  rebar_git_resource:make_vsn(Dir).

make_vsn(Dir,Arg) ->
  io:format("WHAT vsn2\n"),
  %% Extract a version number from the application. This is useful
  %% when defining the version in the .app.src file as `{version, Type}',
  %% which means it should be derived from the build information. For
  %% the `git' resource, this means looking for the last tag and adding
  %% commit-specific information
  rebar_git_resource:make_vsn(Dir,Arg).
  

needs_update(AppInfo, ResourceState) ->
  %io:format("WHAT update\n"),
  %% Extract the Source tuple if needed
  SourceTuple = rebar_app_info:source(AppInfo),
  %% Base version in the current file
  OriginalVsn = rebar_app_info:original_vsn(AppInfo),
  %% Check if the copy in the current install matches
  %% the defined value in the source tuple. On a conflict,
  %% return `true', otherwise `false'
  %rebar_git_resource:needs_update(AppInfo, ResourceState),
  false.


repo_matches_pkg(Pkg, {Pkg,_}) -> true;
repo_matches_pkg(_, {Pkg,_}) -> false.


repo_description_contains_pkg(Pkg, {_,[_,{"release",[{"packages",Pkgs}|_]}, _, _]}) ->
  lists:member(Pkg, Pkgs);
repo_description_contains_pkg(_,_) ->
  false.

get_source_url_from_repo_tuple({_,[_, _, {"source",ATTR_L}, _]}) ->
  [URL] = [ URL || {"url", URL} <- ATTR_L],
  %io:format("~p\n", [URL]),
  URL.

find_repo_for_pkg(Pkg,CustomState) ->
  [[_,{"repositories",REPOS},_,_]] = maps:get(foxy_distro, CustomState),
  %io:format("~p\n",[REPOS]),
  % io:format("~p\n",[Pkg]),
  case [R || R <- REPOS, repo_description_contains_pkg(Pkg,R)] of
    [R] -> get_source_url_from_repo_tuple(R);
    [] -> case [R || R <- REPOS, repo_matches_pkg(Pkg,R)] of
          [R] ->get_source_url_from_repo_tuple(R);
          [] -> rebar_api:warn("Package ~p could not be found in the the downloaded official distribution.yaml",[Pkg]),
                not_found
      end
  end.


modify_app_info_for_git(AppInfo,CustomState) ->
  case rebar_app_info:source(AppInfo) of
    {ros2, foxy} -> ok;
    {ros2, D} -> rebar_api:warn("Ros Distro ~p not supported... but should be fine...",[D])
  end,
  % io:format("YO\n"),
  case find_repo_for_pkg(binary_to_list(rebar_app_info:name(AppInfo)),CustomState) of
    not_found -> skip_dependancy;
    URL -> %io:format(URL),
     rebar_app_info:source(AppInfo, {git, URL , {branch, "master"}})
  end.


% is_pkg_in_currend_repo(Dir,AppName) ->
%   filelib:is_file(filename:join([Dir,AppName,"package.xml"])).


% name_to_dep_tuple(Dir,URL,AppName) ->
%   case is_pkg_in_currend_repo(Dir,AppName) of
%     true -> "{"++AppName++",{ros2, \""++URL++"\",{branch,\"master\"}}}";
%     false -> "{"++AppName++",{ros2, \""++find_repo_for_pkg(AppName)++"\",{branch,\"master\"}}}"
%   end.

add_deps(Dir, AppName, CustomState) ->
  PackageFile = case filelib:is_file(filename:join(Dir,"package.xml")) of
    true -> filename:join(Dir,"package.xml");
    false -> filename:join([Dir,AppName,"package.xml"])
  end,

  % parsing package.xml
  % io:format(PackageFile++"\n"),
  {ok, String} = file:read_file(PackageFile),
  {ok,MP} = re:compile("<\\?xml-model .*\n"),
  RemovedModelLine = re:replace(binary_to_list(String),MP,"",[global,{return,list}]),
  
  {Xml,_} = xmerl_scan:string(RemovedModelLine),   
  Content = Xml#xmlElement.content,        
  PkgDependencies = lists:flatten([X#xmlElement.content || X <- Content, X#xmlElement.name == depend]),
  PkgNames = [X#xmlText.value || X <- PkgDependencies],
  PkgNamesInDistro = [ N || N <- PkgNames, find_repo_for_pkg(N,CustomState) /= not_found],
  %io:format("~p",[PkgNames]),
  string:join([ "{"++N++",{ros2, foxy}}" || N <- PkgNamesInDistro],",\n\t").

convert_repo_to_rebar3_project(Dir, AppInfo, CustomState) -> 
  AppName = binary_to_list(rebar_app_info:name(AppInfo)),
  rebar_api:info("Processing ~p",[AppName]),

  %adding rebar.config
  FilePath = filename:join([Dir, "rebar.config"]),
  rebar_api:info("Adding rebar.config for ~p",[rebar_app_info:name(AppInfo)]),
  file:write_file(FilePath, 
  %"++add_deps(Dir, AppName,rebar_app_info:source(AppInfo)) ++"
  "% generated by "++atom_to_list(?MODULE)++"
  {erl_opts, [debug_info]}.
  {deps, ["++add_deps(Dir, AppName, CustomState)++"]}.


  {plugins, [{rosie_ros2_deps,\"*.\",{git, \"https://github.com/ziopio/rosie_ros2_deps.git\",{branch, \"master\"}}},
        {rosie_interface_compiler,\"*.\",{git, \"https://github.com/ziopio/rosie_interface_compiler.git\",{branch, \"master\"}}}]}.

  {provider_hooks, [
    {pre, [{compile, {rosie, compile}}]},
    {post, [{clean, {rosie, clean}}]}
  ]}."),


  case filelib:is_file(filename:join(Dir,"package.xml")) of
    true -> ok;
    false -> move_interface_files(Dir, AppName)
  end,
  % finally delete the sub directory
  {ok, Listing} = file:list_dir(Dir),
  %io:format(" ~p\n",[Listing]),
  Protected_elements = ["action", "msg", "srv", "rebar.config"],
  [rebar_file_utils:rm_rf(filename:join([Dir, SubDir])) || SubDir <- Listing, not lists:member(SubDir,Protected_elements)],

  % writing basic app.src file
  LibFileName = filename:join([Dir,"src", AppName++".app.src"]),
  rebar_api:info("Adding ~p",[AppName++".app.src"]),
  filelib:ensure_dir(LibFileName),
  file:write_file(LibFileName, "% generated by "++atom_to_list(?MODULE)++"
  {application, "++AppName++",
   [{description, \"Generated app from ros2 pkg: "++AppName++"\"},
    {vsn, \"0.0.0\"},
    {registered, []},
    {applications,
     [kernel,
      stdlib
     ]},
    {env,[]},
    {modules, []},
    {licenses, [\"Apache 2.0\"]},
    {links, []}
   ]}.
  ").
  
move_interface_files(Dir, AppName) ->
  % moving interface files
  ActionFiles = rebar_utils:find_files(filename:join([Dir, AppName, "action"]), ".*\\.action\$"),
  MsgFiles = rebar_utils:find_files(filename:join([Dir, AppName, "msg"]), ".*\\.msg\$"),
  SrvFiles = rebar_utils:find_files(filename:join([Dir, AppName, "srv"]), ".*\\.srv\$"),
  [move_file_to_dir(F,filename:join([Dir,"action"])) || F <- ActionFiles],
  [move_file_to_dir(F,filename:join([Dir,"msg"])) || F <- MsgFiles],
  [move_file_to_dir(F,filename:join([Dir,"srv"])) || F <- SrvFiles].



move_file_to_dir(Filename, Dst) ->
  rebar_api:info("Moving ~p",[Filename]),
  Name = filename:basename(Filename),
  NewFilename = filename:join([Dst,Name]),
  rebar_api:info("Dst:  ~p",[NewFilename]),
  filelib:ensure_dir(NewFilename),
  rebar_file_utils:mv(Filename, NewFilename).



% get_final_dir(FullDir) -> 
%   [F|_] = lists:reverse(filename:split(FullDir)), 
%   F.

% generate_src(AppDir) ->
%     rebar_api:info("Processing ~p\n",[AppDir]),
%     AppName = get_final_dir(AppDir),
%     LibFileName = filename:join([AppDir,"src", AppName++".app.src"]),  
%     rebar_api:info("Adding ~p\n",[AppName++".app.src"]),
%     filelib:ensure_dir(LibFileName),
%     file:write_file(LibFileName, "{application, "++AppName++",
%     [{description, \"Generated app from ros2 pkg: "++AppName++"\"},
%      {vsn, \"0.0.0\"},
%      {registered, []},
%      {applications,
%       [kernel,
%        stdlib,unique_identifier_msgs
%       ]},
%      {env,[]},
%      {modules, []},
%      {licenses, [\"Apache 2.0\"]},
%      {links, []}
%     ]}.
%    ").