--!strict
--[[
    author: Henry Shaw
    description: takes in a resistor mesh/net/graph (idk) and reduces it to the smallest? number of equivalent resistors?
]]
local process = require("@lune/process")
local fs = require("@lune/fs")

-- CONFIG --
local PRINT_DEBUG = false

-- UTILITY --
local function is_dbg()
    return PRINT_DEBUG
end

local function print_dbg(...)
    if PRINT_DEBUG then
        print(...)
    end
end

local function map_concat(t)
    local str = ""
    for k, v in t do
        str = str .. string.format("[%s] = %s, ", k, v)
    end
    return str
end

local function process_concat(t, f)
    local str = ""
    for k, v in t do
        str = str .. string.format("%s,", f(k, v))
    end
    return str
end

-- ResGraph MODULE --
type Index = number

type ComponentCommon = {
    name: string,                   -- symbol of the component (J_, R_, etc)
    adjacencies: {Index},
}

type Junction = ComponentCommon & {
    kind: "junction",
}

type Resistor = ComponentCommon & {
    kind: "resistor",
    r: number,                      -- resistance amount (ohms)
    _reduced: boolean?,
}

type Component = Junction | Resistor

type ResGraph = {
    components: {Component},
    req_name_reg: number,
}

type ResGraphParser = () -> (boolean, {string})

local ResGraph = {}

-- component FUNCTIONS --

local function symbol_type(symbol: string): ("resistor" | "junction" | "unknown")
    if symbol:sub(1, 1) == "R" then
        return "resistor"
    elseif symbol:sub(1, 1) == "J" or symbol == "IN" or symbol == "OUT" then
        return "junction"
    else
        return "unknown"
    end
end

--[[
    desc: overwrite the adjacencies of a component
]]
local function set_adjs(component: Component, adjs: {Index})
    -- type wrangling
    local c = component :: any
    c.adjacencies = adjs
end

--[[
    desc: remove the adjacency from the component
]]
local function remove_adj(component: Component, adj: Index)
    local i = table.find(component.adjacencies, adj)
    if i then
        table.remove(component.adjacencies, i)
    else
        error(`failure: tried to remove not-present adjacency {adj} in component {component.name}`)
    end
end

--[[
    desc: add an adjacency to the junction
    precond: the adjacency is not already present
]]
local function junction_add_adj(component: Junction, adj: Index)
    table.insert(component.adjacencies, adj)
end

--[[
     desc: generically replace an adjacency for a component
]]
local function replace_adj(component: Component, old_adj: Index, new_adj: Index)
    local i = table.find(component.adjacencies, old_adj)
    if i then
        component.adjacencies[i] = new_adj
    else
        error (`failure: tried to replace not-present adjacency {old_adj} in component {component.name}`)
    end
end

--[[ 
    desc: transform the adjacencies of a component
    params:
        component: the component to transform
        transform: a map of old adjacencies to new adjacencies
]]
local function transform_adjs(component: Component, transform: {[Index]: Index})
    for i, adj in component.adjacencies do
        component.adjacencies[i] = transform[adj]
    end
end

-- resistor FUNCTIONS --

--[[
    desc: create a new resistor component
    params:
        symbol: the symbol name of the resistor
        r: the resistance amount (ohms)
        j1: the index of the component connected to the first terminal
        j2: the index of the component connected to the second terminal
]]
local function new_resistor(symbol: string, r: number, j1: Index?, j2: Index?): Resistor
    return {
        kind = "resistor" :: "resistor", -- type wrangling
        name = symbol,
        r = r,
        adjacencies = {j1 or -1, j2 or -1}
    }
end

--[[
    desc: create a new junction component
    params:
        symbol: the symbol name of the junction
        ...: the indicies of the components connected to the junction
]]
local function new_junction(symbol, ...): Junction
    return {
        kind = "junction" :: "junction", -- type wrangling
        name = symbol,
        adjacencies = {...}
    }
end

local function to_resistor(component: Component): Resistor?
    return if component.kind == "resistor" then component else nil
end

local function to_junction(component: Component): Junction?
    return if component.kind == "junction" then component else nil
end

local function from_index(components, index): Component
    return components[index] :: Component
end

local function adj1_from_resistor(resistor: Resistor)
    return resistor.adjacencies[1]
end

local function adj2_from_resistor(resistor: Resistor)
    return resistor.adjacencies[2]
end

local function compute_series_req(components: {Component}, series: {Index})
    local r = 0
    for i = 2, #series - 1 do
        -- ASSUME all components reffed in the series array are resistors
        local c = from_index(components, series[i])
        local resistor = assert(to_resistor(c), 
            `compute_series_req: got non-resistor {c.name} in series`)
        r += resistor.r
    end
    return r
end

local function compute_parrallel_req(components: {Component}, parrallel: {Index})
    local g = 0
    for i = 2, #parrallel - 1 do
        local resistor = to_resistor(from_index(components, parrallel[i])) :: Resistor
        g += 1/resistor.r
    end
    return 1/g
end

--[[ 
    desc: collect a series of resistors from a seed resistor
    params:
        components: the array of components
        resistor: the resistor to start the series from
        into: the array to collect the series into
    returns: true if more than 1 resistor is in the series
    precond:
        at r index is a resistor
]]
local function collect_series(components: {Component}, r: Index, into: {Index})
    local prev
    local curr

    local function traverse()
        -- ASSUME? the resistor hasnt already been visited / reduced
        while true do
            local resistor = to_resistor(from_index(components, curr))
            if resistor then
                local adj1, adj2 = adj1_from_resistor(resistor), adj2_from_resistor(resistor)
                if adj1 ~= prev then
                    prev, curr = curr, adj1
                else
                    prev, curr = curr, adj2
                end
            else
                break
            end
            table.insert(into, prev)
        end
    end

    table.insert(into, 0) -- reserve for front junction
    table.insert(into, r)

    -- traverse first direction
    prev, curr = r, adj1_from_resistor(from_index(components, r) :: Resistor)
    traverse()
    -- place junction at front
    into[1] = curr

    -- traverse second direction
    prev, curr = r, adj2_from_resistor(from_index(components, r) :: Resistor)
    traverse()
    -- place junction at end
    table.insert(into, curr)

    return #into >= 4
end

local function collect_parrallel(components: {Component}, r: Index, into: {Index})
    local resistor = to_resistor(from_index(components, r)) :: Resistor

    local adj1, adj2 = adj1_from_resistor(resistor), adj2_from_resistor(resistor)
    local j1 = to_junction(from_index(components, adj1))
    if not j1 then
        return false
    end

    local j2 = to_junction(from_index(components, adj1))
    if not j2 then
        return false
    end
    
    table.insert(into, adj1)
    table.insert(into, r)

    for _, i in j1.adjacencies do
        if i == r then continue end
        local o_resistor = to_resistor(from_index(components, i))
        if o_resistor then
            local o_adj1 = adj1_from_resistor(o_resistor)
            local o_adj2 = adj2_from_resistor(o_resistor)

            if o_adj1 == adj2 or o_adj2 == adj2 then
                table.insert(into, i)
            end
        end
    end
    
    table.insert(into, adj2)

    return #into >= 4
end

local function junction_is_reducable(components: {Component}, junction: Junction): boolean
    for _, adj in junction.adjacencies do
        if to_junction(from_index(components, adj)) then
            return #junction.adjacencies == 2
        end
    end
    return false
end

--[[
    desc: remove junctions with only 2 adjacent components, connecting them directly
    returns: true if any junctions were removed
]]
local function reduce_junctions(components: {Component}): boolean
    local reduced = false
    local n = 0
    local index_transform = table.create(#components, 0)

    for i, component in components do
        local junction = to_junction(component)
        if junction and junction_is_reducable(components, junction) then
            assert(junction, "reduce_junctions: encountered invalid component.")
            local c1 = from_index(components, junction.adjacencies[1])
            local c2 = from_index(components, junction.adjacencies[2])
            replace_adj(c1, i, junction.adjacencies[2])
            replace_adj(c2, i, junction.adjacencies[1])
            reduced = true
        else
            n += 1
            index_transform[i] = n
        end
        -- TODO: in wierd case where a junction is isolated remove it? (or make an error in a previous stage)
    end

    -- note: this relies on the previous preserving ordering of components and only introducing gaps
    for i, component in components do
        local new_i = index_transform[i]
        if new_i > 0 then
            components[new_i] = component
            transform_adjs(component, index_transform)
        end
    end

    for i = #components, n + 1, -1 do
        components[i] = nil
    end

    return reduced
end

local function reduce_series(graph: ResGraph): boolean
    local components = graph.components
    local reduced = false
    local n = 0
    local index_transform = table.create(#components, 0)
    local series = {}
    
    for i, component in components do
        local resistor = to_resistor(component)

        if resistor and resistor._reduced then
            print_dbg("skipping reduced resistor: ", resistor.name)
            continue
        end

        if resistor then
            if collect_series(components, i, series) then
                print_dbg(process_concat(series, function(_, index) 
                    return from_index(components, index).name
                end))
                reduced = true

                local r = compute_series_req(components, series)
                local adj1_new = series[1]                      -- index of junction at first end of series
                local adj2_new = series[#series]                -- index of junction at second end of series 

                for i = 2, #series - 1 do
                    local s_resistor = assert(to_resistor(from_index(components, series[i])), "reduce series: got non-resistor in series.")
                    s_resistor._reduced = true
                end

                graph.req_name_reg += 1
                local new_resistor = new_resistor("R+"..tostring(graph.req_name_reg), r, adj1_new, adj2_new)
                components[i] = new_resistor
                
                -- repoint adjacencies
                replace_adj(from_index(components, adj1_new), series[2], i)
                replace_adj(from_index(components, adj2_new), series[#series - 1], i)                
            end
            table.clear(series)
        end

        n += 1
        index_transform[i] = n
    end

    -- note: this relies on the previous preserving ordering of components and only introducing gaps
    for i, component in components do
        local new_i = index_transform[i]
        if new_i > 0 then
            components[new_i] = component
            transform_adjs(component, index_transform)
        end
    end

    for i = #components, n + 1, -1 do
        components[i] = nil
    end

    return reduced
end

local function reduce_parrallel(graph: ResGraph)
    local components = graph.components
    local reduced = false
    local n = 0
    local index_transform = table.create(#components, 0)
    local parrallel = {}

    for i, component in components do
        local resistor = to_resistor(component)

        if resistor and resistor._reduced then
            continue
        end

        if resistor then
            if collect_parrallel(components, i, parrallel) then
                print_dbg(table.concat(parrallel, " "))
                print_dbg(process_concat(parrallel, function(_, index) 
                    return from_index(components, index).name
                end))
                reduced = true

                local r = compute_parrallel_req(components, parrallel)
                local j1 = from_index(components, parrallel[1]) :: Junction     -- index of junction at first end of parrallel
                local j2 = from_index(components, parrallel[#parrallel  ]) :: Junction     -- index of junction at second end of parrallel 

                for i = 2, #parrallel - 1 do
                    local p_resistor = assert(to_resistor(from_index(components, parrallel[i])), "reduce parrallel: got non-resistor in parrallel.")
                    p_resistor._reduced = true
                end

                graph.req_name_reg += 1
                local new_resistor = new_resistor("R|"..tostring(graph.req_name_reg), r, parrallel[1], parrallel[#parrallel])
                components[i] = new_resistor
                
                -- repoint adjacencies
                for i = 2, #parrallel - 1 do
                    remove_adj(j1, parrallel[i])
                    remove_adj(j2, parrallel[i])
                end
                junction_add_adj(j1, i)
                junction_add_adj(j2, i)

                table.clear(parrallel)
            end
            table.clear(parrallel)
        end

        n += 1
        index_transform[i] = n
    end

    -- note: this relies on the previous preserving ordering of components and only introducing gaps
    for i, component in components do
        local new_i = index_transform[i]
        if new_i > 0 then
            components[new_i] = component
            transform_adjs(component, index_transform)
        end
    end

    for i = #components, n + 1, -1 do
        components[i] = nil
    end

    return reduced
end

--[[
    desc: get parsers from resgraph file contents
    params:
        file_str: the string contents of the file
        debug: whether to print debug info
    returns: a generator functions which returns at eof or a line of symbols
]]
function ResGraph.parser(file_str: string, debug: boolean?): ResGraphParser
    -- generator
    local routine = coroutine.create(function() 
        local symbols = {}
        for line_str in file_str:gmatch("[^\n]+") do
            -- empty case
            if not line_str:match("%S") then
                continue
            end
            -- comment case
            if line_str:sub(1, 1) == "#" then
                continue
            end
            if debug then
                print_dbg("parse step. line: ", line_str)
            end
            -- parse line
            for symbol in line_str:gmatch("%S+") do
                table.insert(symbols, symbol)
            end

            coroutine.yield(symbols)
            table.clear(symbols)
        end
    end)

    -- generator invoker
    return function()
        local neof, symbols = coroutine.resume(routine)
        return coroutine.status(routine) == "dead" or not neof, symbols
    end
end

function ResGraph.load(parser: ResGraphParser): ResGraph
    local symbols_lookup: {[string]: number}        -- map of names (junction vs resistor) to indicies in the component table
    local components: {Component}                   -- array of loaded components
    local component_symbols: {{string}}             -- array of symbols for each component's connections

    symbols_lookup = {}
    components = {}
    component_symbols = {}

    -- PASS 1 -- 
    local eof, line_symbols = parser()
    while not eof do
        local declared = line_symbols[1]
        if symbols_lookup[declared] then
            error(`duplicate symbol {declared} declared.`)
        end

        local declared_type = symbol_type(declared)
        local component: Component
        local this_component_symbols = {}
        if declared_type == "resistor" then
            local r = assert(tonumber(line_symbols[2]), `resistor {declared} is missing or has invalid resistance value.`)
            local j1_symbol = assert(line_symbols[3], `resistor {declared} is missing 1st connection symbol.`)
            local j2_symbol = assert(line_symbols[4], `resistor {declared} is missing 2nd connection symbol.`)
            this_component_symbols = {j1_symbol, j2_symbol}
            component = new_resistor(declared, r, -1, -1)
        elseif declared_type == "junction" then
            component = new_junction(declared)
            for i = 2, #line_symbols do
                local j_symbol = line_symbols[i]
                table.insert(this_component_symbols, j_symbol)
                table.insert((component :: Junction).adjacencies, -1)
            end
        else
            error(`invalid declared symbol {declared}, expected J__ or R__.`)
        end

        local index = #components + 1
        components[index] = component
        component_symbols[index] = this_component_symbols
        symbols_lookup[declared] = index

        eof, line_symbols = parser()
    end

    -- PASS 2 --
    for i, component in components do
        local this_component_symbols = component_symbols[i]
        local resistor: Resistor?
        local junction: Junction?

        resistor = to_resistor(component)
        junction = to_junction(component)

        if resistor then
            print_dbg(resistor.name)
            print_dbg("adj symbols:", table.concat(this_component_symbols, " "))
            resistor.adjacencies[1] = assert(symbols_lookup[this_component_symbols[1]], `resistor {resistor.name} has undeclared connection: {this_component_symbols[1]}.`)
            resistor.adjacencies[2] = assert(symbols_lookup[this_component_symbols[2]], `resistor {resistor.name} has undeclared connection: {this_component_symbols[2]}.`)
            -- print_dbg("plugged adjs:", process_concat(resistor.adjacencies, function(_, index)
            --     return from_index(components, index).name 
            -- end))
        elseif junction then
            for i, j_symbol in this_component_symbols do
                local j = assert(symbols_lookup[j_symbol], `junction {junction.name} has undeclared connection: {j_symbol}.`)
                junction.adjacencies[i] = j
            end
            junction.adjacencies[#this_component_symbols + 1] = nil -- for some reason iterating w/ out this loops forever
        end
    end

    -- PASS 3 --
    for i, component in components do
        local resistor: Resistor?
        local junction: Junction?

        resistor = to_resistor(component)
        junction = to_junction(component)

        if resistor then
            -- connect resistors to eachother if input contents only declared connection on one side
            print_dbg(i, resistor.name)
            print_dbg("adj indicies:", table.concat(resistor.adjacencies, " "))
            local adj1 = adj1_from_resistor(resistor)
            local adj2 = adj2_from_resistor(resistor)
            assert(not(adj1 == i or adj2 == i), `resistor {resistor.name} is connected to itself.`)

            local toj1 = to_junction(from_index(components, adj1))
            if toj1 then
                junction_add_adj(toj1, i)
            end

            local toj2 = to_junction(from_index(components, adj2))
            if toj2 then
                junction_add_adj(toj2, i)
            end
        elseif junction then
            -- i dont think we need to auto-specify which junctions are connected to eachother
        end
    end

    -- DEBUG --
    print_dbg("post load:")
    print_dbg("symbol_lookup:", map_concat(symbols_lookup))

    -- CONSTRUCT
    local graph: ResGraph = {
        components = components,
        req_name_reg = 0,
    }
    return graph
end

function ResGraph.print_components(graph: ResGraph)
    for i, component in graph.components do
        local resistor = to_resistor(component)
        local junction = to_junction(component)
        if resistor then
            print(`resistor {resistor.name}\t- resistance: {resistor.r}, adj1: {from_index(graph.components, resistor.adjacencies[1]).name}, adj2: {from_index(graph.components, resistor.adjacencies[2]).name}`)
        elseif junction then
            local j_list_str = ""
            for _, j in junction.adjacencies do
                j_list_str = j_list_str .. from_index(graph.components, j).name .. ", "
            end
            print(`junction {junction.name}\t- adjacencies: {j_list_str}`)
        end
    end
end

function ResGraph.clone(graph: ResGraph): ResGraph
    local ngraph = table.clone(graph)
    for i, c in ngraph.components do
        local nc = table.clone(c)
        set_adjs(nc, table.clone(nc.adjacencies))
        ngraph.components[i] = nc
    end
    return ngraph
end

function ResGraph.reduce(graph: ResGraph): ResGraph
    print_dbg("REDUCING GRAPH")
    -- TODO: clone the graph and return the graph
    graph = ResGraph.clone(graph)

    local reduced
    repeat
        reduced = false
        print_dbg("REDUCE SERIES STEP")
        reduced = reduced or reduce_series(graph)
        print_dbg("REDUCE PARRALLEL STEP")
        reduced = reduced or reduce_parrallel(graph)
        print_dbg("REDUCE JUNCTIONS STEP")
        reduced = reduced or reduce_junctions(graph.components)

        print_dbg("PASS COMPLETE, graph:")
        if is_dbg() then ResGraph.print_components(graph) end
        print_dbg("")
    until not reduced

    return graph
end

-- main MODULE --
local function _test_parser()
    print("attempting to parse file: input/circuit1.resgraph")
    local parser = ResGraph.parser(fs.readFile("input/circuit1.resgraph"), false)

    local eof, symbols = parser()
    while not eof do
        print("line symbols:", symbols)
        eof, symbols = parser()
    end
end

local function _test_collect_series()
    local function test_for(components, seed)
        local series = {}
        collect_series(components, seed, series)
        print("collected series result:")
        print(process_concat(series, function(_, index) 
            return from_index(components, index).name
        end))
    end

    do
        local components: {Component} = {
            [1] = new_junction("A", 3, 6),
            [2] = new_junction("B", 5, 6),
            [3] = new_resistor("R1", 0, 1, 4),
            [4] = new_resistor("R2", 0, 3, 5),
            [5] = new_resistor("R3", 0, 4, 6),
            [6] = new_resistor("R4", 0, 5, 2)
        }

        print("case 1:")
        test_for(components, 5)
    end
    
    do
        local components: {Component} = {
            new_junction("A", 3, 5),
            new_junction("B", 4, 5),
            new_resistor("R1", 5, 1, 4),
            new_resistor("R2", 5, 3, 2),
            new_resistor("R3", 5, 1, 2),
        }

        print("case 2:")
        test_for(components, 3)
    end
end

local function _test_collect_parrallel()
    local components: {Component} = {
        new_junction("A", 4, 5, 6),
        new_junction("B", 4, 5),
        new_junction("C", 6),
        new_resistor("R1", 0, 1, 2),
        new_resistor("R2", 0, 1, 2),
        new_resistor("R3", 0, 1, 3),
    }

    local parrallel = {} 
    collect_parrallel(components, 4, parrallel)
    print("collected parrallel result:")
    print(process_concat(parrallel, function(_, index) 
            return from_index(components, index).name
    end))
end

local function _test_reduce_junctions()
    -- TODO: remake to respect new component internal scheme
    -- print("case 1: series of 4 junctions")
    -- local components = {
    --     new_junction("A"),
    --     new_junction("B"),
    --     new_junction("C"),
    --     new_junction("D"),
    -- }
    -- components[1].jlist = {2}
    -- components[2].jlist = {1, 3}
    -- components[3].jlist = {2, 4}
    -- components[4].jlist = {3}
    
    -- print("before reduction:")
    -- ResGraph.print_components({components = components})
    -- print("\nafter reduction:")
    -- reduce_junctions(components)
    -- ResGraph.print_components({components = components})
end

local function _test_reduce_series()
    local components: {Component} = {
        new_junction("A", 3, 5),
        new_junction("B", 4, 5),
        new_resistor("R1", 5, 1, 4),
        new_resistor("R2", 5, 3, 2),
        new_resistor("R3", 5, 1, 2),
    }
    local graph: ResGraph = {
        components = components,
        req_name_reg = 0,
    }
    print("graph before reduction:")
    ResGraph.print_components(graph)
    print("reducing series ...")
    local reduced = reduce_series(graph)
    print("reduced: ", reduced)
    print("graph after reduction:")
    ResGraph.print_components(graph)
end

local function _test_reduce_parrallel()
    local function for_case(components: {Component})
        local graph: ResGraph = {
            components = components,
            req_name_reg = 0,
        }
        print("graph before reduction:")
        ResGraph.print_components(graph)
        print("reducing parrallel ...")
        local reduced = reduce_parrallel(graph)
        print("reduced: ", reduced)
        print("graph after reduction:")
        ResGraph.print_components(graph)
    end

    do
        local components: {Component} = {
            new_junction("A", 4, 5, 6),
            new_junction("B", 4, 5),
            new_junction("C", 6),
            new_resistor("R1", 1, 1, 2),
            new_resistor("R2", 1, 1, 2),
            new_resistor("R3", 0, 1, 3),
        }
        print("case 1:")
        for_case(components)
    end

    do
        local components: {Component} = {
            new_junction("A", 3, 5),
            new_junction("B", 4, 5),
            new_resistor("R1", 1, 1, 4),
            new_resistor("R2", 1, 3, 2),
            new_resistor("R3", 1, 1, 2),
        }
        print("case 2:")
        for_case(components)
    end
end

-- MAIN --
local path = process.args[1]
if #path == 0 then
    print("no resgraph path provided.")
    print("usage: lune eq_resistance_solver.luau <path>")
    process.exit(1)
end

local graph = ResGraph.load(ResGraph.parser(fs.readFile(path), false))
print("loaded:")
ResGraph.print_components(graph)

local graph_reduced = ResGraph.reduce(graph)
print("reduced:")
ResGraph.print_components(graph_reduced)
process.exit(0)