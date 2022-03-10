-module(rosie_ros2_resource).

-behaviour(rebar_resource_v2).

-export([init/2, lock/2, download/4, needs_update/2, make_vsn/2]).

-include_lib("xmerl/include/xmerl.hrl").


download_distro() ->
    case
        httpc:request(
            get,
            {"https://raw.githubusercontent.com/ros/rosdistro/master/galactic/distribution.yaml", []},
            [
                {ssl, [
                    {verify, verify_peer},
                    {cacerts, certifi:cacerts()},
                    {customize_hostname_check, [
                        {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                    ]}
                ]}
            ],
            []
        )
    of
        {ok, {_, _, Body}} ->
            application:start(yamerl),
            yamerl_constr:string(Body);
        {error, Reason} ->
            {error, Reason}
    end.

%% Initialize the custom dep resource plugin
init(Type, _RebarState) ->
    case download_distro() of
        {error, Reason} ->
            rebar_api:error("Failed to gather info for ros distro, Reason: ~p\n", [Reason]);
        Distro ->
            {ok, rebar_resource_v2:new(Type, ?MODULE, #{galactic_distro => Distro})}
    end.

lock(AppInfo, _CustomState) ->
    %io:format("WHAT LOCK?!\n"),
    %% Extract info such as {Type, ResourcePath, ...} as declared
    %% in rebar.config
    %SourceTuple = rebar_git_subdir_resource:lock(modify_app_info_for_git(AppInfo), CustomState),
    %% Annotate and modify the source tuple to make it absolutely
    %% and indeniably unambiguous (for example, with git this means
    %% transforming a branch name into an immutable ref)
    %% Return the unambiguous source tuple
    rebar_app_info:source(AppInfo).

% download(TmpDir, AppInfo, RebarState) ->
%   io:format("WHAT ???????????????\n"),
%         %% Extract info such as {Type, ResourcePath, ...} as declared
%         %% in rebar.config
%         %rebar_git_resource:download(TmpDir, modify_app_info_for_git(AppInfo,CustomState), RebarState),
%         %% Download the resource defined by SourceTuple, which should be
%         %% an OTP application or library, into TmpDir
%         ok.

download(TmpDir, AppInfo, RebarState, CustomState) ->
    case modify_app_info_for_git(AppInfo, CustomState) of
        skip_dependancy ->
            rebar_api:warn("Skipping this dependancy", []),
            ok;
        ModAppInfo ->
            case rebar_git_resource:download(TmpDir, ModAppInfo, RebarState, CustomState) of
                ok ->
                    convert_repo_to_rebar3_project(TmpDir, AppInfo, CustomState),
                    ok;
                {error, Reason} ->
                    rebar_api:error("Git dep failed with error: ~p", [Reason]),
                    {error, Reason}
            end
    end.

% make_vsn(Dir) ->
%   io:format("WHAT vsn1\n"),
%   %% Extract a version number from the application. This is useful
%   %% when defining the version in the .app.src file as `{version, Type}',
%   %% which means it should be derived from the build information. For
%   %% the `git' resource, this means looking for the last tag and adding
%   %% commit-specific information
%   rebar_git_resource:make_vsn(Dir).

make_vsn(Dir, Arg) ->
    io:format("WHAT vsn2\n"),
    %% Extract a version number from the application. This is useful
    %% when defining the version in the .app.src file as `{version, Type}',
    %% which means it should be derived from the build information. For
    %% the `git' resource, this means looking for the last tag and adding
    %% commit-specific information
    rebar_git_resource:make_vsn(Dir, Arg).

needs_update(_AppInfo, _ResourceState) ->
    %io:format("WHAT update\n"),
    %% Extract the Source tuple if needed
    %   SourceTuple = rebar_app_info:source(AppInfo),
    %% Base version in the current file
    %   OriginalVsn = rebar_app_info:original_vsn(AppInfo),
    %% Check if the copy in the current install matches
    %% the defined value in the source tuple. On a conflict,
    %% return `true', otherwise `false'
    %rebar_git_resource:needs_update(AppInfo, ResourceState),
    false.

repo_matches_pkg(Pkg, {Pkg, _}) ->
    true;
repo_matches_pkg(_, {_Pkg, _}) ->
    false.

repo_description_contains_pkg(
    Pkg,
    {_, [_, {"release", [{"packages", Pkgs} | _]}, _, _]}
) ->
    lists:member(Pkg, Pkgs);
repo_description_contains_pkg(_, _) ->
    false.

get_source_url_from_repo_tuple({_, [_, _, {"source", ATTR_L}, _]}) ->
    [URL] = [URL || {"url", URL} <- ATTR_L],
    %io:format("~p\n", [URL]),
    URL.

find_repo_for_pkg(Pkg, CustomState) ->
    [[_, {"repositories", REPOS}, _, _]] = maps:get(galactic_distro, CustomState),
    %io:format("~p\n",[REPOS]),
    % io:format("~p\n",[Pkg]),
    case [R || R <- REPOS, repo_description_contains_pkg(Pkg, R)] of
        [R] ->
            get_source_url_from_repo_tuple(R);
        [] ->
            case [R || R <- REPOS, repo_matches_pkg(Pkg, R)] of
                [R] ->
                    get_source_url_from_repo_tuple(R);
                [] ->
                    rebar_api:warn(
                        "Package ~p could not be found in the the downloaded official distribution.yaml",
                        [Pkg]
                    ),
                    not_found
            end
    end.

modify_app_info_for_git(AppInfo, CustomState) ->
    Branch = case rebar_app_info:source(AppInfo) of
        {ros2, galactic, {branch, B}} -> 
            B;
        {ros2, galactic} -> 
            default;
        {ros2, D} ->
            rebar_api:warn("Ros Distro ~p not supported... but should be fine...", [D])
    end,
    REPO = find_repo_for_pkg(binary_to_list(rebar_app_info:name(AppInfo)), CustomState),
    case {REPO,Branch} of
        {not_found,_} ->
            skip_dependancy;
        {URL, default} ->
            rebar_app_info:source(AppInfo, {git, URL});
        {URL, Branch} ->
            rebar_app_info:source(AppInfo, {git, URL, {branch, Branch}})
    end.

% is_pkg_in_currend_repo(Dir,AppName) ->
%   filelib:is_file(filename:join([Dir,AppName,"package.xml"])).

% name_to_dep_tuple(Dir,URL,AppName) ->
%   case is_pkg_in_currend_repo(Dir,AppName) of
%     true -> "{"++AppName++",{ros2, \""++URL++"\",{branch,\"master\"}}}";
%     false -> "{"++AppName++",{ros2, \""++find_repo_for_pkg(AppName)++"\",{branch,\"master\"}}}"
%   end.

find_deps(Dir, AppName, CustomState) ->
    PackageFile =
        case
            filelib:is_file(
                filename:join(Dir, "package.xml")
            )
        of
            true ->
                filename:join(Dir, "package.xml");
            false ->
                filename:join([Dir, AppName, "package.xml"])
        end,

    % parsing package.xml
    % io:format(PackageFile++"\n"),
    {ok, String} = file:read_file(PackageFile),
    {ok, MP} = re:compile("<\\?xml-model .*\n"),
    RemovedModelLine = re:replace(binary_to_list(String), MP, "", [global, {return, list}]),

    {Xml, _} = xmerl_scan:string(RemovedModelLine),
    Content = Xml#xmlElement.content,
    PkgDependencies =
        lists:flatten([X#xmlElement.content || X <- Content, (X#xmlElement.name == depend) or (X#xmlElement.name == build_depend)]),
    PkgNames = [X#xmlText.value || X <- PkgDependencies],
    PkgNamesInDistro = [N || N <- PkgNames, find_repo_for_pkg(N, CustomState) /= not_found],
    %io:format("~p",[PkgNames]),
    PkgNamesInDistro.

convert_repo_to_rebar3_project(Dir, AppInfo, CustomState) ->
    AppName = binary_to_list(rebar_app_info:name(AppInfo)),
    rebar_api:info("Processing ~p", [AppName]),

    %adding rebar.config
    FilePath = filename:join([Dir, "rebar.config"]),
    rebar_api:info("Adding rebar.config for ~p", [rebar_app_info:name(AppInfo)]),
    Dependencies = find_deps(Dir, AppName, CustomState),
    Deps = string:join(["{" ++ A ++ ",{ros2, galactic}}" || A <- Dependencies], ",\n\t"),
    file:write_file(
        FilePath,
        %"++add_deps(Dir, AppName,rebar_app_info:source(AppInfo)) ++"
        "% generated by " ++ atom_to_list(?MODULE) ++
            "\n"
            "{erl_opts, [debug_info]}.\n"
            "{deps, [" ++ Deps ++
            "]}.\n"
            "\n"
            "\n"
            "{plugins, [{rosie_ros2_deps,\"*.\",{git, \"https://github.com/rosie-project/rosie_ros2_deps.git\",{branch, \"master\"}}},\n"
            "        {rosie_interface_compiler,\"*.\",{git, \"https://github.com/rosie-project/rosie_interface_compiler.git\",{branch, \"master\"}}}]}.\n"
            "\n"
            "{provider_hooks, [\n"
            "    {pre, [{compile, {rosie, compile}}]},\n"
            "    {post, [{clean, {rosie, clean}}]}\n"
            "  ]}."
    ),

    case
        filelib:is_file(
            filename:join(Dir, "package.xml")
        )
    of
        true ->
            ok;
        false ->
            move_interface_files(Dir, AppName)
    end,
    % finally delete the sub directory
    {ok, Listing} = file:list_dir(Dir),
    %io:format(" ~p\n",[Listing]),
    Protected_elements = ["action", "msg", "srv", "rebar.config"],
    [
        rebar_file_utils:rm_rf(
            filename:join([Dir, SubDir])
        )
     || SubDir <- Listing, not lists:member(SubDir, Protected_elements)
    ],

    % writing basic app.src file
    LibFileName = filename:join([Dir, "src", AppName ++ ".app.src"]),
    DependencyApps = string:join(Dependencies, ",\n\t"),
    rebar_api:info("Adding ~p", [AppName ++ ".app.src"]),
    filelib:ensure_dir(LibFileName),
    file:write_file(
        LibFileName,
        "% generated by " ++ atom_to_list(?MODULE) ++
            "\n"
            "{application, " ++ AppName ++
            ",\n"
            "\t[{description, \"Generated app from ros2 pkg: " ++ AppName ++
            "\"},\n"
            "\t{vsn, \"0.0.0\"},\n"
            "\t{registered, []},\n"
            "\t{applications,\n"
            "\t\t[kernel,\n"
            "\t\tstdlib"
            ++
            case length(DependencyApps) > 0 of
                true -> (",\n\t\t"++DependencyApps);
                false -> ""
            end
            ++
            "\n\t]},\n"
            "\t{env,[]},\n"
            "\t{modules, []},\n"
            "\t{licenses, [\"Apache 2.0\"]},\n"
            "\t{links, []}\n"
            "]}.\n"
    ).

move_interface_files(Dir, AppName) ->
    % moving interface files
    ActionFiles =
        rebar_utils:find_files(
            filename:join([Dir, AppName, "action"]), ".*\\.action\$"
        ),
    MsgFiles =
        rebar_utils:find_files(
            filename:join([Dir, AppName, "msg"]), ".*\\.msg\$"
        ),
    SrvFiles =
        rebar_utils:find_files(
            filename:join([Dir, AppName, "srv"]), ".*\\.srv\$"
        ),
    [move_file_to_dir(F, filename:join([Dir, "action"])) || F <- ActionFiles],
    [move_file_to_dir(F, filename:join([Dir, "msg"])) || F <- MsgFiles],
    [move_file_to_dir(F, filename:join([Dir, "srv"])) || F <- SrvFiles].

move_file_to_dir(Filename, Dst) ->
    rebar_api:info("Moving ~p", [Filename]),
    Name = filename:basename(Filename),
    NewFilename = filename:join([Dst, Name]),
    rebar_api:info("Dst:  ~p", [NewFilename]),
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
