include("fsm.jl")
include("files_api.jl")

dfa = FSM.parse_fsm(FilesApi.read_from_file("fsm.txt"))

for i in 1:parse(Int, ARGS[1])
    try
        w = FSM.gen_random_word_in_language(dfa)
        println("in=", w)
    catch e
    end
end
for i in 1:parse(Int, ARGS[1])
    w = FSM.gen_random_word_not_in_language(dfa)
    println("notin=", w)
end