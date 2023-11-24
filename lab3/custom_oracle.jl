include("files_api.jl")
include("fsm.jl")
using .FilesApi: read_from_file, parse_inputs
using .FSM: is_in_language, gen_random_word_in_language, parse_fsm
function is_not_p(lp, ls, p, oracle, word, P, P1)
    if is_in_language(lp, p)
        for i in 1:P
            w5 = gen_random_word_in_language(ls)
            for i in 0:P1-1
                test_word = p*word[2]^i*word[3]*word[4]^i*w5
                res = occursin(oracle, test_word)
                if !res
                    @goto yes
                end
            end
        end
        return false
        @label yes
        return true
    else
        return false
    end
end
function is_not_s(lp, ls, s, oracle, word, P, P1)
    if is_in_language(ls, s)
        for i in 1:P
            w1 = gen_random_word_in_language(lp)
            for i in 0:P1-1
                test_word = w1*word[2]^i*word[3]*word[4]^i*s
                res = occursin(oracle, test_word)
                if !res
                    @goto yes
                end
            end
        end
        return false
        @label yes
        return true
    else
        return false
    end
end
oracle, C, P, P1, alphabet, word, for_part = parse_inputs(read_from_file("instructions.txt"))
oracle = Regex(oracle[1])
lp = parse_fsm(read_from_file("lp.txt"))
ls = parse_fsm(read_from_file("ls.txt"))
str = isempty(ARGS) ? "" : ARGS[1]
if for_part == "p"
    println(is_not_p(lp, ls, str, oracle, word, P, P1))
else
    println(is_not_s(lp, ls, str, oracle, word, P, P1))
end