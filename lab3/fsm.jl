module FSM
struct State
    number
    is_initial
    is_accepting
    to_transitions
    from_transitions
end

struct Fsm
    initial
    accepting
    alphabet
    states
    tramsitions
end


function parse_fsm(lines)
    transitions = []
    initial = 0
    accepting = []
    alphabet = Set()
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
                transitions = [[[] for _ in 1:states_num] for _ in 1:states_num]
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
                push!(transitions[from_state][to_state], by_term)
            end
        end
    end
    transitions = gen_trap(transitions, alphabet)
    return Fsm(initial, accepting, alphabet, transform_transitions_to_states(initial, accepting, transitions),
    transitions)
end

function transform_transitions_to_states(initial, accepting, transitions)
    states = []
    to_transitions = []
    from_transitions = [[] for _ in eachindex(transitions)]
    for i in eachindex(transitions)
        carrent_state_to_transitions = []
        for j in eachindex(transitions[i])
            if !isempty(transitions[i][j])
                push!(carrent_state_to_transitions, [j, transitions[i][j]])
                push!(from_transitions[j], [i, transitions[i][j]])
            end
        end
        push!(to_transitions, carrent_state_to_transitions)
    end
    for i in eachindex(transitions)
        push!(states, State(i, i==initial, i in accepting, to_transitions[i], from_transitions[i]))
    end
    return states
end

function gen_trap(transitions, alphabet)
    for i in transitions
        terms = []
        for j in i
            terms = [terms; j]
        end
        if !isempty(setdiff(alphabet, terms))
            @goto gen
        end
    end
    return transitions
    @label gen
    missing_terms = []
    for i in transitions
        terms = []
        for j in i
            terms = [terms; j]
        end
        push!(missing_terms, setdiff(alphabet, terms))
    end
    for i in eachindex(transitions)
        push!(transitions[i], [j for j in missing_terms[i]])
    end
    push!(transitions, [[] for _ in 1:length(transitions)+1])
    transitions[end][end] = [i for i in alphabet]
    return transitions
end

function gen_random_word_in_language(fsm)
    word = ""
    if isempty(fsm.accepting)
        return false
    end
    current_state = fsm.states[rand(fsm.accepting)]
    while true
        if current_state.is_initial == true
            if rand([false, true])
                break
            end
        end
        if !isempty(current_state.from_transitions)
            transition = rand(current_state.from_transitions)
            from_state = transition[1]
            by_term = rand(transition[2])
            word *= by_term
            current_state = fsm.states[from_state]
        else
            return false
        end
    end
    return reverse(word)
end
function gen_random_word_not_in_language(fsm)
    word = ""
    not_accepting_states = setdiff(1:length(fsm.tramsitions), fsm.accepting)
    if isempty(not_accepting_states)
        return false
    end
    current_state = fsm.states[rand(not_accepting_states)]
    while true
        if current_state.is_initial == true
            if rand([false, true])
                break
            end
        end
        if !isempty(current_state.from_transitions)
            transition = rand(current_state.from_transitions)
            from_state = transition[1]
            by_term = rand(transition[2])
            word *= by_term
            current_state = fsm.states[from_state]
        else 
            return false
        end
    end
    return reverse(word)
end
function is_in_language(fsm, word)
    current_state = fsm.states[fsm.initial]
    if word != ""
        for term in Vector{String}(split(word, ""))
            for j in current_state.to_transitions
                if term in j[2]
                    @goto ok
                end
            end
            return false
            @label ok
        end
    end
    if current_state.is_accepting
        return true
    else
        return false
    end
    
end
function not_fsm(fsm)
    not_accepting = setdiff(1:length(fsm.tramsitions), fsm.accepting)
    if isempty(not_accepting)
        return false
    else
        return Fsm(fsm.initial, not_accepting, fsm.alphabet,
         transform_transitions_to_states(fsm.initial, not_accepting, fsm.tramsitions), fsm.tramsitions)
    end
end

function Fsm_to_string(fsm)
    res = "#states\n$(length(fsm.tramsitions))\n#initial\n$(fsm.initial)\n#accepting\n"
    for i in fsm.accepting
        res *= "$i\n"
    end
    res *= "#alphabet\n"
    for i in fsm.alphabet
        res *= "$i\n"
    end
    res *= "#transitions\n"
    for i in fsm.states
        for j in i.to_transitions
            for k in j[2]
                res *= "$(i.number):$k>$(j[1])\n"
            end
        end
    end
    return replace(res, r"\n$" => "")
end
end