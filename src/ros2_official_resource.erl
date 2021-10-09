-module(ros2_official_resource).
-behaviour(rebar_resource_v2).
-export([init/2,
         lock/2,
         download/4, download/3,
         needs_update/2,
         make_vsn/1,
         make_vsn/2]).



%% Initialize the custom dep resource plugin
init(Type, _RebarState) ->
   CustomState = #{},
   Resource = rebar_resource_v2:new(
       Type,         % type tag such as 'git' or 'hg'
       ?MODULE,      % this callback module
       CustomState   % anything you want to carry around for next calls
   ),
   {ok, Resource}.

lock(AppInfo, CustomState) ->
  io:format("WHAT LOCK?!\n"),
  %% Extract info such as {Type, ResourcePath, ...} as declared
  %% in rebar.config
  SourceTuple = rebar_git_subdir_resource:lock(modify_app_info_for_git(AppInfo), CustomState),
  %% Annotate and modify the source tuple to make it absolutely
  %% and indeniably unambiguous (for example, with git this means
  %% transforming a branch name into an immutable ref)

  %% Return the unambiguous source tuple
  ModifiedSource= SourceTuple.

download(TmpDir, AppInfo, CustomState) ->
  io:format("WHAT\n"),
        %% Extract info such as {Type, ResourcePath, ...} as declared
        %% in rebar.config
        rebar_git_subdir_resource:download(TmpDir, modify_app_info_for_git(AppInfo), CustomState),
        %% Download the resource defined by SourceTuple, which should be
        %% an OTP application or library, into TmpDir
        ok.

modify_app_info_for_git(AppInfo) ->
  {ros2, URL, Branch, SubDir} = rebar_app_info:source(AppInfo),
  rebar_app_info:source(AppInfo, {git_subdir, URL, Branch, SubDir}).

download(TmpDir, AppInfo, CustomState, RebarState) ->
  ModAppInfo = modify_app_info_for_git(AppInfo),
  case rebar_git_subdir_resource:download(TmpDir, ModAppInfo, CustomState, RebarState) of
    ok -> %convert_repo_to_rebar3_project(TmpDir,AppInfo), 
          ok;
    {error, Reason} -> rebar_api:error("Git dep failed with error: \n",[Reason]), {error, Reason}
  end.

make_vsn(Dir) ->
  io:format("WHAT\n"),
  %% Extract a version number from the application. This is useful
  %% when defining the version in the .app.src file as `{version, Type}',
  %% which means it should be derived from the build information. For
  %% the `git' resource, this means looking for the last tag and adding
  %% commit-specific information
  rebar_git_subdir_resource:make_vsn(Dir).

make_vsn(Dir,Arg) ->
  io:format("WHAT\n"),
  %% Extract a version number from the application. This is useful
  %% when defining the version in the .app.src file as `{version, Type}',
  %% which means it should be derived from the build information. For
  %% the `git' resource, this means looking for the last tag and adding
  %% commit-specific information
  rebar_git_subdir_resource:make_vsn(Dir,Arg).
  

needs_update(AppInfo, ResourceState) ->
  io:format("WHAT\n"),
  %% Extract the Source tuple if needed
  SourceTuple = rebar_app_info:source(AppInfo),
  %% Base version in the current file
  OriginalVsn = rebar_app_info:original_vsn(AppInfo),
  %% Check if the copy in the current install matches
  %% the defined value in the source tuple. On a conflict,
  %% return `true', otherwise `false'
  rebar_git_subdir_resource:needs_update(AppInfo, ResourceState).

convert_repo_to_rebar3_project(Dir, AppInfo) -> 
  rebar_api:info("Hi!").
  % AppDirs = [ filename:dirname(Path) || Path <- rebar_utils:find_files(Dir,"package\.xml")],
  % rebar_api:info("Found packages: ~p\n",[AppDirs]),
  % [generate_src(D) || D <- AppDirs],
  
  % FilePath = filename:join([Dir, "rebar.config"]),
  % rebar_api:info("Adding rebar.config for ~p\n",[rebar_app_info:name(AppInfo)]),
  % file:write_file(FilePath, 
  % "% generated
  % {erl_opts, [debug_info]}.
  % {deps, []}.
  % {project_app_dirs, [" ++string:join([ "\""++get_final_dir(D)++"\"" || D <- AppDirs],",")++"]}.\n"),

  % FilePath2 = filename:join([Dir, "rebar.lock"]),
  % rebar_api:info("Adding rebar.lock for ~p\n",[rebar_app_info:name(AppInfo)]),
  % file:write_file(FilePath2, 
  % "[].").

get_final_dir(FullDir) -> 
  [F|_] = lists:reverse(filename:split(FullDir)), 
  F.

generate_src(AppDir) ->
    % rebar_api:info("Processing ~p\n",[AppDir]),
    AppName = get_final_dir(AppDir),
    LibFileName = filename:join([AppDir,"src", AppName++".app.src"]),  
    rebar_api:info("Adding ~p\n",[AppName++".app.src"]),
    filelib:ensure_dir(LibFileName),
    file:write_file(LibFileName, "{application, "++AppName++",
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
