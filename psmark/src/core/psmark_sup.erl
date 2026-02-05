%%%-------------------------------------------------------------------
%% @doc top-level supervisor
%%%-------------------------------------------------------------------
-module(psmark_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(NodeName, NodeList) ->
    supervisor:start_link({global, NodeName}, ?MODULE, [{NodeName, NodeList}]).

init([{NodeName, NodeList}]) ->
    Pg = #{id => pg_srv, start => {pg, start_link, []},
        restart => permanent, shutdown => 5000, type => worker, modules => [pg]},

    Lifecycle = #{id => psmark_lifecycle,
        start => {psmark_lifecycle, start_link, [NodeList, 100000]},
        restart => permanent, shutdown => 5000, type => worker, modules => [psmark_lifecycle]},

    NodeManager = #{id => psmark_node_manager,
        start => {psmark_node_manager, start_link, [NodeName]},
        restart => permanent, shutdown => 5000, type => worker, modules => [psmark_node_manager]},

    ScenarioSup = #{id => psmark_scenario_sup,
        start => {psmark_scenario_sup, start_link, []},
        restart => permanent, shutdown => 5000, type => supervisor, modules => [psmark_scenario_sup]},

    HwStats = #{id => psmark_metrics_hw_stats_reader,
        start => {psmark_metrics_hw_stats_reader, start_link, []},
        restart => permanent, shutdown => 5000, type => worker, modules => [psmark_metrics_hw_stats_reader]},

    Children = case psmark_config_manager:using_hw_poll() of
        true ->
            [Pg, Lifecycle, NodeManager, ScenarioSup, HwStats];
        false ->
            [Pg, Lifecycle, NodeManager, ScenarioSup]
    end,
    {ok, {{one_for_one, 5, 60}, Children}}.

