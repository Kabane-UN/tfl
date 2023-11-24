module FilesApi
function read_from_file(file)
    lines = []
    open(file, "r") do f
        for i in readlines(f)
            push!(lines, i)
        end
    end
    return lines
end
function write_to_file(file, str)
    open(file, "w") do f
        print(f, str)
    end
end
function parse_inputs(lines)
    C = NaN
    P = NaN
    P1 = NaN
    oracle = []
    alphabet = Set()
    word = []
    count = 1
    for_part = ""
    flag = ""
    for line in lines
        if line == "#oracle"
            flag = "oracle"
        elseif line == "#const"
            flag = "const"
        elseif line == "#alphabet"
            flag = "alphabet"
        elseif line == "#split"
            flag = "split"
        elseif line == "#for"
            flag = "for"
        else
            if flag == "oracle"
                push!(oracle, line)
            elseif flag == "const"
                num = parse(Int, line)
                if count == 1
                    C = num
                elseif count == 2
                    P = num
                elseif count == 3
                    P1 = num
                end
                count += 1
            elseif flag == "alphabet"
                push!(alphabet, line)
            elseif flag == "split"
                push!(word, line)
                if length(word) == 5
                    break
                end
            elseif flag == "for"
                for_part = line
            end
        end
    end
    return oracle, C, P, P1, alphabet, word, for_part
end
end