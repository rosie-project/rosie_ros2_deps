-module(rosie_dep_manager).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok,rebar_state:add_resource(State, {ros2, ros2_official_resource})}.