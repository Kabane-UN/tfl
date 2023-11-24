include("fsm.jl")
include("files_api.jl")
using .FilesApi: read_from_file, write_to_file, parse_inputs
using .FSM: is_in_language, gen_random_word_in_language, not_fsm, parse_fsm, Fsm_to_string

mutable struct Multy_transition
    from
    by
    to
end
mutable struct Short_fsm
    initial
    accepting
    alphabet
    states
    multy_transitions
end
function gen_oracle(regex)
    return "println(occursin(r\"$regex\", ARGS[1]))"
end
function gen_instructions(oracle, C, P, P1, alphabet, word, for_part)
    res = "#oracle\n$oracle\n#const\n$C\n$P\n$P1\n#alphabet\n"
    for term in alphabet
        res *= "$term\n"
    end
    res *= "#for\n$for_part\n"
    res *= "#split\n"
    for i in word
        res *= "$i\n"
    end
    return res
end
function gen_com(oracle, for_part, C, P, P1, alphabet, word=C_NULL)
    res = "#oracle\n$oracle\n#for\n$for_part\n#const\n$C\n$P\n$P1\n#alphabet\n"
    for (i, term) in enumerate(alphabet)
        res *= "$term"
        if i == length(alphabet)
            res *= "\n"
        end
    end
    if for_part != "n"
        res *= "#split\n"
        for i in word
            res *= "$i\n"
        end
    end
    return res
end
function get_multy_transition(multy_transitions, from, by)
    for i in eachindex(multy_transitions)
        if multy_transitions[i].from == from && multy_transitions[i].by == by
            return i
        end
    end
    return 0
end
function parse_Short_fsm(lines)
    states = []
    initial = 0
    accepting = []
    alphabet = Set()
    multy_transitions = []
    flag = ""
    for line in lines
        if line == "#states"
            flag = "states"
        elseif line == "#initial"
            flag = "initial"
        elseif line == "#accepting"
            flag = "accepting"
        elseif line == "#alphabet"
            flag = "alphabet"
        elseif line == "#transitions"
            flag = "transitions"
        else
            if flag == "states"
                states_num = parse(Int, line)
                states = [i for i in 1:states_num]
            elseif flag == "initial"
                initial = parse(Int, line)+1
            elseif flag == "accepting"
                push!(accepting, parse(Int, line)+1)
            elseif flag == "alphabet"
                push!(alphabet, line)
            elseif flag == "transitions"
                sub_lines = split(line, ":")
                from_state = parse(Int, sub_lines[1])+1
                sub_lines = split(sub_lines[2], ">")
                by_term = sub_lines[1]
                to_state = parse(Int, sub_lines[2])+1
                i = get_multy_transition(multy_transitions, from_state, by_term)
                if i == 0
                    push!(multy_transitions, Multy_transition(from_state, by_term, [to_state]))
                else
                    push!(multy_transitions[i].to, to_state)
                end
            end
        end
    end
    return Short_fsm(initial, accepting, alphabet, states, multy_transitions)
end
function transform_transitions_to_multy_stansitions(alphabet, transitions)
    multy_transitions = []
    for i in eachindex(transitions)
        current_multy_transitions = Dict([(term, []) for term in alphabet])
        for j in eachindex(transitions[i])
            for term in transitions[i][j]
                if !(j in current_multy_transitions[term])
                    push!(current_multy_transitions[term], j)
                end
            end
        end
        for term in keys(current_multy_transitions)
            if !isempty(current_multy_transitions[term])
                push!(multy_transitions, Multy_transition(i, term, current_multy_transitions[term]))
            end
        end
    end
    return multy_transitions
end
function Fsm_to_Short_fsm(fsm)
    return Short_fsm(fsm.initial, fsm.accepting, fsm.alphabet,
     [i for i in eachindex(fsm.tramsitions)], transform_transitions_to_multy_stansitions(fsm.alphabet, fsm.tramsitions))
end
function find_by_multy_transitions(multy_transitions, from, by)
    for multy_transition in multy_transitions
        if multy_transition.from == from && multy_transition.by == by
            return multy_transition.to
        end
    end
    return []
end
function gen_tram_for_Short_fsm(fsm)
    need_trap = false
    initial = fsm.initial
    states = copy(fsm.states)
    accepting = fsm.accepting
    multy_transitions = deepcopy(fsm.multy_transitions)
    for state in eachindex(fsm.states)
        for term in fsm.alphabet
            for multy_transition in fsm.multy_transitions
                if multy_transition.from == state && multy_transition.by == term
                    @goto found
                end
            end
            if !need_trap
                push!(states, length(fsm.states)+1)
                need_trap = true
                for i in fsm.alphabet
                    push!(multy_transitions, Multy_transition(length(fsm.states)+1, i, [length(fsm.states)+1]))
                end
            end
            push!(multy_transitions, Multy_transition(state, term, [length(fsm.states)+1]))
            @label found
        end
    end
    return Short_fsm(initial, accepting, fsm.alphabet, states, multy_transitions)
end
function not_Short_fsm(fsm)
    not_accepting = setdiff(fsm.states, fsm.accepting)
    if isempty(not_accepting)
        return false
    else
        return Short_fsm(fsm.initial, not_accepting, fsm.alphabet, fsm.states, fsm.multy_transitions)
    end
end
function Short_fsm_to_string(fsm)
    res = "#states\n$(length(fsm.states))\n#initial\n$(fsm.initial)\n#accepting\n"
    for i in fsm.accepting
        res *= "$i\n"
    end
    res *= "#alphabet\n"
    for i in fsm.alphabet
        res *= "$i\n"
    end
    res *= "#transitions\n"
    for multy_transition in fsm.multy_transitions
        for to in multy_transition.to
            res *= "$(multy_transition.from):$(multy_transition.by)>$to\n"
        end
    end
    return replace(res, r"\n$" => "")
end
function union_Short_fsm(fsm1, fsm2)
    alphabet = union(fsm1.alphabet, fsm2.alphabet)
    initial = 1
    len_states = length(fsm1.states)
    accepting = [[i+1 for i in fsm1.accepting];[i+1+len_states for i in fsm2.accepting]]
    states = [[i+1 for i in fsm1.states];[i+1+len_states for i in fsm2.states]]
    multy_transitions = [Multy_transition(initial, "ϵ", [fsm1.initial+1, fsm2.initial+1+len_states])]
    for multy_transition in fsm1.multy_transitions
        push!(multy_transitions, Multy_transition(multy_transition.from+1, multy_transition.by, [to+1 for to in multy_transition.to]))
    end
    for multy_transition in fsm2.multy_transitions
        push!(multy_transitions, Multy_transition(multy_transition.from+1+len_states, multy_transition.by, [to+1+len_states for to in multy_transition.to]))
    end
    return Short_fsm(initial, accepting, alphabet, states, multy_transitions)
end
function computeEpsilon(fsm, states)
    res_states = []
    while !isempty(states)
        current_state = pop!(states)
        push!(res_states, current_state)
        for multy_transition in fsm.multy_transitions
            if multy_transition.by == "ϵ" && current_state == multy_transition.from
                for state in multy_transition.to
                    if state in res_states && state in states
                        continue
                    end
                    push!(states, state)
                end
            end
        end
    end
    return res_states
end
function build_simple_transition(fsm, states, term)
    res_states = []
    for multy_transition in fsm.multy_transitions
        if multy_transition.by == term && multy_transition.from in states
            for i in multy_transition.to
                if !(i in res_states)
                    push!(res_states, i)
                end
            end
        end
    end
    return res_states
end
function build_transition(fsm, states, term)
    states = computeEpsilon(fsm, copy(states))
    states = build_simple_transition(fsm, states, term)
    return computeEpsilon(fsm, states)

end
function remove_epsilon(fsm)
    initial = fsm.initial
    accepting = copy(fsm.accepting)
    alphabet = copy(fsm.alphabet)
    initialEpsilon = computeEpsilon(fsm, [fsm.initial])
    if !(fsm.initial in fsm.accepting)
        for i in initialEpsilon
            if i in accepting
                push!(accepting, initial)
                break
            end
        end
    end
    new_multy_transitions = []
    multy_states_transitions = []
    for i in fsm.states
        for term in fsm.alphabet
            to_states = sort(build_transition(fsm, [i], term))
            if !isempty(to_states)
                push!(new_multy_transitions, Multy_transition(i, term, to_states))
            end
        end
    end
    for i in eachindex(new_multy_transitions)
        if length(new_multy_transitions[i].to) > 1
            for j in multy_states_transitions
                if j == new_multy_transitions[i].to
                    new_multy_transitions[i].to = j
                    @goto exist
                end
            end
            push!(multy_states_transitions, new_multy_transitions[i].to)
            @label exist
        end
    end
    return Short_fsm(initial, accepting, alphabet, fsm.states, new_multy_transitions)
end
function to_numeric_states(fsm)
    numeric_states = [i for i in eachindex(fsm.states)]
    initial = findfirst(x -> x == fsm.initial, fsm.states)
    accepting = sort([findfirst(x -> x == i, fsm.states) for i in fsm.accepting])
    multy_transitions = []
    for i in eachindex(fsm.multy_transitions)
        from = findfirst(x -> x == fsm.multy_transitions[i].from, fsm.states)
        to = sort([findfirst(x -> x == i, fsm.states) for i in fsm.multy_transitions[i].to])
        push!(multy_transitions, Multy_transition(from, fsm.multy_transitions[i].by, to))
    end
    return Short_fsm(initial, accepting, fsm.alphabet, numeric_states, multy_transitions)
end

function determine(fsm)
    initial = [fsm.initial]
    accepting = [[i] for i in fsm.accepting]
    alphabet = fsm.alphabet
    multy_transitions = []
    states = [[i] for i in fsm.states]
    new_states = []
    for multy_transition in fsm.multy_transitions
        new_multy_transition = Multy_transition([multy_transition.from], multy_transition.by, [multy_transition.to])
        if length(new_multy_transition.to[1]) > 1 && !(new_multy_transition.to[1] in new_states)
            push!(new_states, new_multy_transition.to[1])
        end
        push!(multy_transitions, new_multy_transition)
    end
    while !isempty(new_states)
        state = pop!(new_states)
        push!(states, state)
        for i in state
            if i in fsm.accepting
                push!(accepting, state)
                break
            end
        end
        for term in alphabet
            to = sort(build_transition(fsm, state, term))
            flag1 = 0
            flag2 = 0
            for i in states
                if i == to
                    flag1+=1
                    break
                end
            end
            for i in new_states
                if i == to
                    flag2+=1
                    break
                end
            end
            if !isempty(to)
                push!(multy_transitions, Multy_transition(state, term, [to]))
            end
            if flag1 == 0 && flag2 == 0 && length(to)>1
                push!(new_states, to)
            end
        end
    end
    return to_numeric_states(Short_fsm(initial, accepting, alphabet, states, multy_transitions))
end
function find_reachable(fsm, state)
    states = [state]
    reachable_states = [state]
    while !isempty(states)
        current = pop!(states)
        for multy_transition in fsm.multy_transitions
            if current == multy_transition.from
                for to_state in multy_transition.to
                    if !(to_state in reachable_states)
                        push!(reachable_states, to_state)
                        if !(to_state in states)
                            push!(states, to_state)
                        end
                    end
                end
            end
        end
    end
    return reachable_states
end
function remove_unreachable(fsm)
    reachable_states = find_reachable(fsm, fsm.initial)
    accepting = filter(x -> x in reachable_states, fsm.accepting)
    multy_transitions = filter(x -> x.from in reachable_states, fsm.multy_transitions)
    initial = fsm.initial
    alphabet = fsm.alphabet
    states = filter(x -> x in reachable_states, fsm.states)
    return to_numeric_states(Short_fsm(initial, accepting, alphabet, states, multy_transitions))
end
function are_equivalent(fsm, state1, state2)
    to_do_pairs = [[state1, state2]]
    do_pairs = []
    while !isempty(to_do_pairs)
        current = pop!(to_do_pairs)
        predicate1 = current[1] in fsm.accepting
        predicate2 = current[2] in fsm.accepting
        if predicate1 != predicate2
            return false
        end
        push!(do_pairs, current)
        for term in fsm.alphabet
            new_pair = [build_transition(fsm, [current[1]], term), build_transition(fsm, [current[2]], term)]
            if !isempty(new_pair[1]) && !isempty(new_pair[2])
                new_pair = [new_pair[1][1], new_pair[2][1]]
                if !(new_pair in do_pairs) && !(new_pair in to_do_pairs)
                    push!(to_do_pairs, new_pair)
                end
            end
        end
    end
    return true
end
function factor_by_equivalent(fsm)
    function is_in_pair(state, equivalent_pairs)
        for pair in equivalent_pairs
            if state == pair[2]
                return true
            end
        end
        return false
    end
    function get_equivalent(state, equivalent_pairs)
        for pair in equivalent_pairs
            if state == pair[2]
                return pair[1]
            end
        end
        return state
    end
    equivalent_pairs = []
    alphabet = fsm.alphabet
    for i in eachindex(fsm.states)
        for j in i+1:length(fsm.states)
            if are_equivalent(fsm, i, j)
                new_pair = [i, j]
                if !(new_pair in equivalent_pairs)
                    push!(equivalent_pairs, new_pair)
                end
            end
        end
    end
    states = filter(x -> ! is_in_pair(x, equivalent_pairs), fsm.states)
    accepting = filter(x -> ! is_in_pair(x, equivalent_pairs), fsm.accepting)
    initial = get_equivalent(fsm.initial, equivalent_pairs)
    multy_transitions = []
    for multy_transition in fsm.multy_transitions
        if !is_in_pair(multy_transition.from, equivalent_pairs) 
            push!(multy_transitions, Multy_transition(multy_transition.from, multy_transition.by, [get_equivalent(to, equivalent_pairs) for to in multy_transition.to]))
        end
    end
    return to_numeric_states(Short_fsm(initial, accepting, alphabet, states, multy_transitions))
end
function minimize(fsm)
    fsm = remove_unreachable(fsm)
    return factor_by_equivalent(fsm)
end
function gen_result(fsm1, fsm2)
    union_alphabet = union(fsm1.alphabet, fsm2.alphabet)
    fsm1 = Short_fsm(fsm1.initial, fsm1.accepting, union_alphabet, fsm1.states, fsm1.multy_transitions)
    fsm2 = Short_fsm(fsm2.initial, fsm2.accepting, union_alphabet, fsm2.states, fsm2.multy_transitions)
    fsm1 = gen_tram_for_Short_fsm(fsm1)
    fsm1 = gen_tram_for_Short_fsm(fsm2)
    fsm1 = not_Short_fsm(fsm1)
    res_fsm = C_NULL
    if fsm1 == false
        res_fsm = fsm2
        @goto det
    end
    res_fsm = union_Short_fsm(fsm1, fsm2)
    res_fsm = remove_epsilon(res_fsm)
    @label det
    res_fsm = determine(res_fsm)
    res_fsm = gen_tram_for_Short_fsm(res_fsm)
    res_fsm = not_Short_fsm(res_fsm)
    if res_fsm == false
        return "ø"
    else
        res_fsm = minimize(res_fsm)
        return Short_fsm_to_string(res_fsm)
    end
end
macro run_l_star()
    return :(Sys.iswindows() ? run(`main.exe ./com.txt ./com.txt`) : run(`./main ./com.txt ./com.txt`))
end

begin
    res = "-- Prefix\nø\n-- Infix\nø\n-- Postfix\nø"
    input_lines = read_from_file("input.txt")
    oracle, C, P, P1, alphabet, word, _ = parse_inputs(input_lines)
    regex = Regex(oracle[1])
    for i in 0:P1-1
        s = word[1]*word[2]^i*word[3]*word[4]^i*word[5]
        result = occursin(regex, s)
        if !result
            println("Разбиение не выдержало накачку")
            @goto ex
        end
    end
    write_to_file("oracle.jl", gen_oracle(oracle[2]))
    write_to_file("com.txt", gen_com("oracle.jl", "p", C, P, P1, alphabet, word))
    @run_l_star
    lp = parse_fsm(read_from_file("com.txt"))
    write_to_file("oracle.jl", gen_oracle(oracle[3]))
    write_to_file("com.txt", gen_com("oracle.jl", "s", C, P, P1, alphabet, word))
    @run_l_star
    ls = parse_fsm(read_from_file("com.txt"))
    write_to_file("oracle.jl", gen_oracle(oracle[1]))
    write_to_file("com.txt", gen_com("oracle.jl", "i", C, P, P1, alphabet, word))
    @run_l_star
    li = parse_fsm(read_from_file("com.txt"))
    counterexamples = []
    for _ in 1:(C + length(ls.tramsitions) + length(lp.tramsitions))
        w1 = gen_random_word_in_language(lp)
        w5 = gen_random_word_in_language(ls)
        for i in 0:P1
            s = w1*word[2]^i*word[3]*word[4]^i*w5
            result = occursin(regex, s)
            if !result
                push!(counterexamples, (w1, w5))
                break
            end
        end
        if length(counterexamples) > (C + length(ls.tramsitions) + length(lp.tramsitions))
            break
        end
    end
    if length(counterexamples) > (C + length(ls.tramsitions) + length(lp.tramsitions))
        global res
        write_to_file("lp.txt", Fsm_to_string(lp))
        write_to_file("ls.txt", Fsm_to_string(ls))
        write_to_file("instructions.txt", gen_instructions(oracle[1], C, P, P1, alphabet, word, "p"))
        write_to_file("com.txt", gen_com("custom_oracle.jl", "n", C, P, P1, alphabet))
        @run_l_star
        not_lp = parse_Short_fsm(read_from_file("com.txt"))
        write_to_file("instructions.txt", gen_instructions(oracle[1], C, P, P1, alphabet, word, "s"))
        write_to_file("com.txt", gen_com("custom_oracle.jl", "n", C, P, P1, alphabet))
        @run_l_star
        not_ls = parse_Short_fsm(read_from_file("com.txt"))
        lp = Fsm_to_Short_fsm(lp)
        ls = Fsm_to_Short_fsm(ls)
        res = "-- Prefix\n$(gen_result(lp, not_lp))\n-- Infix\n$(Fsm_to_string(li))\n-- Postfix\n$(gen_result(ls, not_ls))"
    else
        global res
        res = "-- Prefix\n$(Fsm_to_string(lp))\n-- Infix\n$(Fsm_to_string(li))\n-- Postfix\n$(Fsm_to_string(ls))"
    end
    @label ex
    write_to_file("output.txt", res)
end