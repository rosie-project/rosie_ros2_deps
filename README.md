rosie_ros2_deps
=====

A rebar plugin to fetch ROS packages and repos for ROSIE;

Internally this plugin uses rebar_git_resource to fetch the ROS repositories.

Build
-----

    rebar3 compile

How to add a ros package to your rebar3 deps
-----

    % rebar.config

    {deps, [ { ros_pkg_name, {ros2, foxy}} ]}.

How It Works
-----

- The pluging will search for a ros package for the foxy distribution that matches the app name.

- If it finds one, will fetch it's repository.

- It will extract from the repository all the ros interfaces contained in the package directory; than it will parse its package.xml file to discover its dependancies.

- Eventual dependencies will be listed as above in the erlang.config file of the newly generated app.

- The generated app will use this same plugin so that rebar can chain this operations recursively to satisfy any dependancy tree.

- If one dep has a dependancy not listed in the distribution the missing dep is ignored and not added in the rebar.config file; a warning is printed.

Third party Software
-----

To parse the .yaml file containing all ros2 packages for a distribution this plugin depends on yamerl -> <https://github.com/yakaz/yamerl>
